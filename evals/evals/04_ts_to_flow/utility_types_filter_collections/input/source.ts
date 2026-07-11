/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

type Channel = "email" | "sms" | "push" | "webhook";

type PublicChannel = Exclude<Channel, "webhook">;
type InstantChannel = Extract<Channel, "sms" | "push">;

interface ChannelConfig {
  retries?: number;
  timeoutMs?: number;
}
type ResolvedConfig = Required<ChannelConfig>;

declare function send(channel: PublicChannel): Promise<boolean>;
type SendResult = Awaited<ReturnType<typeof send>>;

type RateLimits = ReadonlyMap<Channel, number>;
type Enabled = ReadonlySet<PublicChannel>;

function pick<T extends PublicChannel>(options: T[], fallback: NoInfer<T>): T {
  return options.includes(fallback) ? fallback : options[0];
}

function describe(
  config: ResolvedConfig,
  limits: RateLimits,
  enabled: Enabled,
): string {
  const instant: InstantChannel = "sms";
  return [
    `retries=${config.retries}`,
    `timeout=${config.timeoutMs}`,
    `smsLimit=${limits.get("sms") ?? 0}`,
    `emailEnabled=${String(enabled.has("email"))}`,
    `instant=${instant}`,
  ].join(" ");
}

const result: SendResult = true;
const chosen = pick(["email", "sms"], "sms");

console.log(
  describe({ retries: 3, timeoutMs: 1000 }, new Map(), new Set()),
  result,
  chosen,
);
