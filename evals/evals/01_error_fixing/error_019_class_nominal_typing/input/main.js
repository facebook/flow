/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

class EmailNotifier {
  sent: number = 0;

  send(message: string): boolean {
    const ok = message.length > 0 && message.length < 500;
    if (ok) {
      this.sent += 1;
    }
    return ok;
  }
}

class SmsNotifier {
  sent: number = 0;

  send(message: string): boolean {
    const ok = message.length > 0 && message.length < 160;
    if (ok) {
      this.sent += 1;
    }
    return ok;
  }
}

function dispatchAll(
  notifier: EmailNotifier,
  messages: ReadonlyArray<string>,
): number {
  let delivered = 0;
  for (const m of messages) {
    if (notifier.send(m)) {
      delivered += 1;
    }
  }
  return delivered;
}

const sms: SmsNotifier = new SmsNotifier();
const delivered: number = dispatchAll(sms, ['hi', 'ping', '']);
console.log(`delivered ${delivered} of 3 via sms`);
