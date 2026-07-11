type Listener = (payload: string) => void;

class EventBus {
  listeners: Listener[] = [];

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
  written: string[] = [];

  handle(payload: string): void {
    const stamped = `[audit] ${payload.toUpperCase()}`;
    this.written.push(stamped);
  }
}

const bus = new EventBus();
const audit = new AuditLogger();
bus.subscribe(audit.handle);

const first = bus.publish("login");
const second = bus.publish("logout");
console.log(`notified ${first + second}, ${audit.written.length} audited`);
