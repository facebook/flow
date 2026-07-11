/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Listener = (payload: string) => void;

class EventBus {
  listeners: Array<Listener> = [];

  subscribe(fn: Listener): void {
    this.listeners.push(fn);
  }

  publish(payload: string): number {
    let notified = 0;
    for (const fn of this.listeners) {
      fn(payload);
      notified += 1;
    }
    return notified;
  }
}

class AuditLogger {
  written: Array<string> = [];

  handle(payload: string): void {
    const stamped = `[audit] ${payload.toUpperCase()}`;
    this.written.push(stamped);
  }
}

const bus: EventBus = new EventBus();
const audit: AuditLogger = new AuditLogger();
bus.subscribe((payload: string) => audit.handle(payload));

const first: number = bus.publish('login');
const second: number = bus.publish('logout');
console.log(`notified ${first + second}, ${audit.written.length} audited`);
