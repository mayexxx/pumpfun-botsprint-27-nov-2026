import { runBot, LAMPORTS_PER_SOL } from './vendor/makefun-bot.ts';

const BUY_LAMPORTS = 50000000n;
const STOP_LOSS_BPS = 1000;

const TAKE_PROFIT_BPS = 3000;
const ENTRY_MOMENTUM_BPS = 800;
const MIN_TRADES_FOR_ENTRY = 3;
const MIN_BUY_LAMPORTS = LAMPORTS_PER_SOL / 100n;
const MAX_POSITIONS = 5;
const MAX_ORDERS_PER_BATCH = 16;
const MAX_SLIPPAGE_BPS = 10000 as const;
const CURVE_FEE_BPS = 125n;
const BPS_DENOM = 10000n;
const STALE_CANDIDATE_SLOTS = 300;
const PUMPFUN_PROGRAM_ID = '6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P';

interface Position {
  token_amount: bigint;
  cost_lamports: bigint;
}

interface CurveState {
  mint: string;
  created_slot: number;
  virtual_token_reserves: bigint;
  virtual_quote_reserves: bigint;
  real_token_reserves: bigint;
  real_quote_reserves: bigint;
  trades: number;
  complete: boolean;
}

interface BotState {
  hello: unknown;
  seq: number;
  slot: number;
  account: unknown;
  cash: bigint;
  positions: Map<string, Position>;
  curves: Map<string, CurveState>;
  pending: Map<string, unknown>;
  clientId(): string;
  txFeeLamports(): bigint;
  maxBuyLamports(mint: string): bigint;
  committedPositionCount(): number;
  freeCashLamports(): bigint;
}

interface Batch {
  seq: number;
  slot: number;
  events?: unknown[];
  marks?: unknown;
  fills?: unknown[];
  account?: unknown;
}

interface BuyOrder {
  side: 'buy';
  client_id: string;
  mint: string;
  quote_lamports: string;
  max_slippage_bps: 10000;
}

interface SellOrder {
  side: 'sell';
  client_id: string;
  mint: string;
  token_amount: string;
  max_slippage_bps: 10000;
}

type Order = BuyOrder | SellOrder;

interface Reserves {
  vq: bigint;
  vt: bigint;
}

interface WatchedCoin {
  mint: string;
  creationPrice: number;
  createdSlot: number;
  lastSlot: number;
  trades: number;
}

const watchedCoins = new Map<string, WatchedCoin>();
const activeCandidates = new Set<string>();
const latestReserves = new Map<string, Reserves>();
const ineligibleMints = new Set<string>();
const completedMints = new Set<string>();
const attemptedBuys = new Set<string>();

function compareAscii(a: string, b: string): number {
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}

function minBigInt(a: bigint, b: bigint): bigint {
  return a < b ? a : b;
}

function toBigInt(val: unknown): bigint {
  if (typeof val === 'bigint') {
    return val;
  }
  if (typeof val === 'number') {
    if (!Number.isFinite(val)) {
      return 0n;
    }
    return BigInt(Math.trunc(val));
  }
  if (typeof val === 'string') {
    const trimmed = val.trim();
    if (/^-?\d+$/.test(trimmed)) {
      return BigInt(trimmed);
    }
  }
  return 0n;
}

function isTruthyFlag(val: unknown): boolean {
  return val === true || val === 1 || val === 'true' || val === '1';
}

function computePrice(vq: bigint, vt: bigint): number {
  if (vq <= 0n || vt <= 0n) {
    return 0;
  }
  const q = Number(vq);
  const t = Number(vt);
  if (!Number.isFinite(q) || !Number.isFinite(t) || t <= 0) {
    return 0;
  }
  return q / t;
}

function isPumpFunProgram(
  data: Record<string, unknown>,
  outer: Record<string, unknown>
): boolean {
  const raw = data.program ?? data.program_id ?? outer.program ?? outer.program_id;
  if (raw === undefined || raw === null || raw === '') {
    return true;
  }
  const text = String(raw).trim();
  if (text === PUMPFUN_PROGRAM_ID) {
    return true;
  }
  const lower = text.toLowerCase();
  return lower === 'pump' || lower === 'pump.fun' || lower === 'pumpfun';
}

function classifyEvent(raw: unknown): {
  kind: 'create' | 'trade' | 'complete' | 'other';
  data: Record<string, unknown>;
  outer: Record<string, unknown>;
} {
  if (!raw || typeof raw !== 'object') {
    return { kind: 'other', data: {}, outer: {} };
  }

  const outer = raw as Record<string, unknown>;

  if (outer.CreateEvent && typeof outer.CreateEvent === 'object') {
    return {
      kind: 'create',
      data: outer.CreateEvent as Record<string, unknown>,
      outer,
    };
  }
  if (outer.TradeEvent && typeof outer.TradeEvent === 'object') {
    return {
      kind: 'trade',
      data: outer.TradeEvent as Record<string, unknown>,
      outer,
    };
  }
  if (outer.CompleteEvent && typeof outer.CompleteEvent === 'object') {
    return {
      kind: 'complete',
      data: outer.CompleteEvent as Record<string, unknown>,
      outer,
    };
  }

  const payload =
    outer.data && typeof outer.data === 'object'
      ? (outer.data as Record<string, unknown>)
      : outer;

  const rawType = outer.type ?? outer.kind ?? outer.event ?? outer.event_type;
  if (typeof rawType === 'string') {
    const lower = rawType.toLowerCase();
    if (lower.includes('create')) {
      return { kind: 'create', data: payload, outer };
    }
    if (lower.includes('trade')) {
      return { kind: 'trade', data: payload, outer };
    }
    if (lower.includes('complete')) {
      return { kind: 'complete', data: payload, outer };
    }
  }

  if (
    'token_total_supply' in payload ||
    'is_mayhem_mode' in payload ||
    'is_cashback_enabled' in payload
  ) {
    return { kind: 'create', data: payload, outer };
  }

  if ('is_buy' in payload || 'sol_amount' in payload) {
    return { kind: 'trade', data: payload, outer };
  }

  return { kind: 'other', data: payload, outer };
}

function readReservesFromObject(obj: unknown): Reserves | null {
  if (!obj || typeof obj !== 'object') {
    return null;
  }
  const rec = obj as Record<string, unknown>;
  const vq = toBigInt(rec.virtual_quote_reserves ?? rec.virtual_sol_reserves);
  const vt = toBigInt(rec.virtual_token_reserves);
  if (vq > 0n && vt > 0n) {
    return { vq, vt };
  }
  return null;
}

function extractMarkMap(marks: unknown): Map<string, Reserves> {
  const result = new Map<string, Reserves>();
  if (!marks || typeof marks !== 'object') {
    return result;
  }

  if (marks instanceof Map) {
    for (const [mint, val] of marks.entries()) {
      if (typeof mint !== 'string') continue;
      const res = readReservesFromObject(val);
      if (res) {
        result.set(mint, res);
      }
    }
    return result;
  }

  if (Array.isArray(marks)) {
    for (let i = 0; i < marks.length; i += 1) {
      const item = marks[i];
      if (!item || typeof item !== 'object') continue;
      const rec = item as Record<string, unknown>;
      if (typeof rec.mint !== 'string') continue;
      const res = readReservesFromObject(rec);
      if (res) {
        result.set(rec.mint, res);
      }
    }
    return result;
  }

  const record = marks as Record<string, unknown>;
  const keys = Object.keys(record);
  for (let i = 0; i < keys.length; i += 1) {
    const mint = keys[i];
    const res = readReservesFromObject(record[mint]);
    if (res) {
      result.set(mint, res);
    }
  }
  return result;
}

function getCurrentReserves(
  mint: string,
  state: BotState,
  markMap: Map<string, Reserves>
): Reserves | null {
  const fromMark = markMap.get(mint);
  if (fromMark) {
    return fromMark;
  }

  const curve = state.curves.get(mint);
  if (curve) {
    const vq = toBigInt(curve.virtual_quote_reserves);
    const vt = toBigInt(curve.virtual_token_reserves);
    if (vq > 0n && vt > 0n) {
      return { vq, vt };
    }
  }

  return latestReserves.get(mint) ?? null;
}

await runBot({
  onBatch(state: BotState, batch: Batch): Order[] {
    const currentSlot = typeof batch.slot === 'number' ? batch.slot : state.slot;
    const events = Array.isArray(batch.events) ? batch.events : [];
    const markMap = extractMarkMap(batch.marks);

    for (const [mint, res] of markMap.entries()) {
      latestReserves.set(mint, res);
    }

    for (let i = 0; i < events.length; i += 1) {
      const parsed = classifyEvent(events[i]);
      const mintRaw = parsed.data.mint ?? parsed.outer.mint;
      if (typeof mintRaw !== 'string' || mintRaw.length === 0) {
        continue;
      }
      const mint = mintRaw;

      if (parsed.kind === 'create') {
        const mayhem = isTruthyFlag(parsed.data.is_mayhem_mode);
        const cashback = isTruthyFlag(parsed.data.is_cashback_enabled);
        const validProgram = isPumpFunProgram(parsed.data, parsed.outer);

        if (mayhem || cashback || !validProgram) {
          ineligibleMints.add(mint);
          watchedCoins.delete(mint);
          activeCandidates.delete(mint);
          continue;
        }

        const res = readReservesFromObject(parsed.data) ?? getCurrentReserves(mint, state, markMap);
        if (!res) {
          continue;
        }

        const creationPrice = computePrice(res.vq, res.vt);
        if (creationPrice <= 0) {
          continue;
        }

        latestReserves.set(mint, res);
        if (!attemptedBuys.has(mint) && !completedMints.has(mint)) {
          watchedCoins.set(mint, {
            mint,
            creationPrice,
            createdSlot: currentSlot,
            lastSlot: currentSlot,
            trades: 0,
          });
        }
      } else if (parsed.kind === 'trade') {
        const res = readReservesFromObject(parsed.data);
        if (res && (watchedCoins.has(mint) || state.positions.has(mint))) {
          latestReserves.set(mint, res);
        }

        const watched = watchedCoins.get(mint);
        if (watched && !ineligibleMints.has(mint) && !attemptedBuys.has(mint) && !completedMints.has(mint)) {
          watched.trades = watched.trades + 1;
          watched.lastSlot = currentSlot;
          activeCandidates.add(mint);
        }
      } else if (parsed.kind === 'complete') {
        completedMints.add(mint);
        watchedCoins.delete(mint);
        activeCandidates.delete(mint);
      }
    }

    const txFee = state.txFeeLamports();
    let freeCash = state.freeCashLamports();
    if (freeCash < 0n) {
      freeCash = 0n;
    }

    const orders: Order[] = [];

    const heldMints = Array.from(state.positions.keys()).sort(compareAscii);
    for (let i = 0; i < heldMints.length; i += 1) {
      if (orders.length >= MAX_ORDERS_PER_BATCH) {
        break;
      }

      const mint = heldMints[i];
      const pos = state.positions.get(mint);
      if (!pos || pos.token_amount <= 0n) {
        continue;
      }

      attemptedBuys.add(mint);

      const curve = state.curves.get(mint);
      const isComplete = completedMints.has(mint) || Boolean(curve && curve.complete);
      if (isComplete) {
        completedMints.add(mint);
      }

      let shouldExit = isComplete;

      if (!shouldExit && pos.cost_lamports > 0n) {
        const res = getCurrentReserves(mint, state, markMap);
        if (res) {
          const price = computePrice(res.vq, res.vt);
          if (price > 0) {
            const marketValue = Number(pos.token_amount) * price;
            const cost = Number(pos.cost_lamports);
            const takeProfitTarget = (cost * (10000 + TAKE_PROFIT_BPS)) / 10000;
            const stopLossTarget = (cost * (10000 - STOP_LOSS_BPS)) / 10000;

            if (marketValue >= takeProfitTarget || marketValue <= stopLossTarget) {
              shouldExit = true;
            }
          }
        }
      }

      if (shouldExit) {
        if (freeCash < txFee && state.cash < txFee) {
          continue;
        }
        orders.push({
          side: 'sell',
          client_id: state.clientId(),
          mint,
          token_amount: pos.token_amount.toString(),
          max_slippage_bps: MAX_SLIPPAGE_BPS,
        });
        freeCash = freeCash > txFee ? freeCash - txFee : 0n;
      }
    }

    let committedPositions = state.committedPositionCount();
    const candidateMints = Array.from(activeCandidates).sort(compareAscii);

    for (let i = 0; i < candidateMints.length; i += 1) {
      const mint = candidateMints[i];
      const watched = watchedCoins.get(mint);

      if (!watched || ineligibleMints.has(mint) || attemptedBuys.has(mint) || completedMints.has(mint)) {
        activeCandidates.delete(mint);
        watchedCoins.delete(mint);
        continue;
      }

      const curve = state.curves.get(mint);
      if (curve && curve.complete) {
        completedMints.add(mint);
        activeCandidates.delete(mint);
        watchedCoins.delete(mint);
        continue;
      }

      if (currentSlot - watched.lastSlot > STALE_CANDIDATE_SLOTS) {
        activeCandidates.delete(mint);
        continue;
      }

      if (orders.length >= MAX_ORDERS_PER_BATCH || committedPositions >= MAX_POSITIONS) {
        continue;
      }

      const existingPos = state.positions.get(mint);
      if (existingPos && existingPos.token_amount > 0n) {
        attemptedBuys.add(mint);
        activeCandidates.delete(mint);
        watchedCoins.delete(mint);
        continue;
      }

      const curveTrades = curve && typeof curve.trades === 'number' ? curve.trades : 0;
      const totalTrades = watched.trades >= curveTrades ? watched.trades : curveTrades;
      if (totalTrades < MIN_TRADES_FOR_ENTRY) {
        continue;
      }

      const res = getCurrentReserves(mint, state, markMap);
      if (!res) {
        continue;
      }

      const currentPrice = computePrice(res.vq, res.vt);
      const entryPriceThreshold = (watched.creationPrice * (10000 + ENTRY_MOMENTUM_BPS)) / 10000;
      if (currentPrice < entryPriceThreshold) {
        continue;
      }

      const maxBuy = state.maxBuyLamports(mint);
      if (maxBuy < MIN_BUY_LAMPORTS) {
        continue;
      }

      const cashAfterTxFee = freeCash > txFee ? freeCash - txFee : 0n;
      const maxAffordable = (cashAfterTxFee * BPS_DENOM) / (BPS_DENOM + CURVE_FEE_BPS);
      let quoteLamports = minBigInt(BUY_LAMPORTS, maxBuy);
      quoteLamports = minBigInt(quoteLamports, maxAffordable);

      if (quoteLamports < MIN_BUY_LAMPORTS) {
        continue;
      }

      orders.push({
        side: 'buy',
        client_id: state.clientId(),
        mint,
        quote_lamports: quoteLamports.toString(),
        max_slippage_bps: MAX_SLIPPAGE_BPS,
      });

      attemptedBuys.add(mint);
      watchedCoins.delete(mint);
      activeCandidates.delete(mint);
      committedPositions = committedPositions + 1;

      const estimatedCurveFee = (quoteLamports * CURVE_FEE_BPS + (BPS_DENOM - 1n)) / BPS_DENOM;
      const totalCashUsed = quoteLamports + txFee + estimatedCurveFee;
      freeCash = freeCash > totalCashUsed ? freeCash - totalCashUsed : 0n;
    }

    for (const mint of latestReserves.keys()) {
      if (!watchedCoins.has(mint) && !state.positions.has(mint)) {
        latestReserves.delete(mint);
      }
    }

    return orders;
  },
  onDone(_state: BotState, _done: unknown): void {},
});
