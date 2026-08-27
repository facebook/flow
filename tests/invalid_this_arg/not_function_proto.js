// The check is type-aware: members named `call`/`apply`/`bind` that resolve on
// the receiver itself, rather than on `Function.prototype`, are untouched.

class Phone {
  call(number: string): void {}
  apply(job: string): void {}
  bind(other: Phone): void {}
}

declare const phone: Phone;
declare const number: string;

phone.call(number); // ok
phone.apply('job'); // ok
phone.bind(phone); // ok

type Callable = {
  call: (x: string) => void,
  apply: (x: string) => void,
  bind: (x: string) => void,
};
declare const c: Callable;

c.call('x'); // ok
c.apply('x'); // ok
c.bind('x'); // ok

declare const anything: any;
anything.call(1); // ok - nothing is resolved on `any`

declare const untyped: $FlowFixMe;
untyped.bind(2); // ok
