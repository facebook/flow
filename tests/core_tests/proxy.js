// @flow

const target = {value: 1};
const handler: Proxy$traps<typeof target> = {
  get(target, key, receiver) {
    key as string | symbol;
    return Reflect.get(target, key, receiver);
  },
  ownKeys() {
    return {0: 'value', 1: Symbol(), length: 2};
  },
};

const proxy = new Proxy(target, handler);
proxy.value as number;
proxy.value as string; // ERROR
proxy as Proxy<typeof target>;

const revocable = Proxy.revocable(target, handler);
revocable as Proxy$revocable<typeof target>;
revocable.proxy.value as number;
revocable.proxy.value as string; // ERROR
revocable.revoke();
revocable.value; // ERROR: properties belong to the wrapped proxy

new Proxy(1, {}); // ERROR: targets must be objects
new Proxy(target, {ownKeys: () => [1]}); // ERROR: keys must be strings or symbols
new Proxy(target, {has: () => 'yes'}); // ERROR: the trap must return a boolean
