type BaseRetry = {
  maxAttempts: number;
  baseDelayMs: number;
  multiplier: number;
};

type JitterConfig = {
  jitterRatio: number;
  seed: number;
};

type EndpointRetry = BaseRetry & JitterConfig & {
  endpoint: string;
  authToken: string;
};

function nextSeed(seed: number): number {
  return (seed * 1103515245 + 12345) & 0x7fffffff;
}

function attemptDelay(
  policy: EndpointRetry,
  attempt: number,
): number {
  let raw = policy.baseDelayMs;
  for (let i = 0; i < attempt; i++) {
    raw = raw * policy.multiplier;
  }
  const seed = nextSeed(policy.seed + attempt);
  const jitter = (seed % 1000) / 1000 * policy.jitterRatio * raw;
  return Math.round(raw + jitter);
}

function planSchedule(policy: EndpointRetry): Array<number> {
  const schedule: Array<number> = [];
  for (let a = 0; a < policy.maxAttempts; a++) {
    schedule.push(attemptDelay(policy, a));
  }
  return schedule;
}

const uploads: EndpointRetry = {
  maxAttempts: 4,
  baseDelayMs: 100,
  multiplier: 2,
  jitterRatio: 0.25,
  seed: 7,
  endpoint: "/uploads",
  authToken: "u-abc",
};

const search: EndpointRetry = {
  maxAttempts: 3,
  baseDelayMs: 50,
  multiplier: 3,
  jitterRatio: 0.1,
  seed: 42,
  endpoint: "/search",
  authToken: "s-xyz",
};

const plans: Array<EndpointRetry> = [uploads, search];
for (const p of plans) {
  const schedule = planSchedule(p);
  const total = schedule.reduce((a, b) => a + b, 0);
  console.log(`${p.endpoint} (${p.authToken}): ${schedule.join(",")} total=${total}`);
}
