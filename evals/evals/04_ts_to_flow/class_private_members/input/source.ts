class RateLimiter {
  private hits: Map<string, {count: number, windowStart: number}> = new Map();

  constructor(private windowMs: number, private maxPerWindow: number) {}

  private rollWindow(clientId: string, now: number): {count: number, windowStart: number} {
    const entry = this.hits.get(clientId);
    if (entry === undefined || now - entry.windowStart >= this.windowMs) {
      const fresh = {count: 0, windowStart: now};
      this.hits.set(clientId, fresh);
      return fresh;
    }
    return entry;
  }

  admit(clientId: string, now: number): boolean {
    const entry = this.rollWindow(clientId, now);
    if (entry.count >= this.maxPerWindow) {
      return false;
    }
    entry.count += 1;
    return true;
  }

  remaining(clientId: string, now: number): number {
    const entry = this.rollWindow(clientId, now);
    return Math.max(0, this.maxPerWindow - entry.count);
  }
}

const limiter = new RateLimiter(1000, 3);
const t = 10_000;
const results: boolean[] = [
  limiter.admit("alice", t),
  limiter.admit("alice", t + 100),
  limiter.admit("alice", t + 200),
  limiter.admit("alice", t + 300),
  limiter.admit("alice", t + 1500),
];
console.log(`alice results: ${results.join(",")}, remaining: ${limiter.remaining("alice", t + 1500)}`);
