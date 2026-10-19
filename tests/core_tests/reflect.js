// @flow

const target = {value: 1};
Reflect.get(target, 'value') as number;
Reflect.get(target, 'value') as string; // ERROR

const descriptor = Reflect.getOwnPropertyDescriptor<typeof target, 'value'>(target, 'value');
descriptor.value; // ERROR: a descriptor can be absent
if (descriptor != null) {
  descriptor.value as number | void;
  if (descriptor.value != null) {
    descriptor.value as string; // ERROR
  }
}

Reflect.ownKeys(target) as Array<string | symbol>;
Reflect.ownKeys(target) as Array<string>; // ERROR

Reflect.getPrototypeOf(target) as interface {} | null;
Reflect.getPrototypeOf(target) as interface {}; // ERROR

Reflect.has(1, 'value'); // ERROR: targets must be objects
Reflect.deleteProperty(target, true); // ERROR: keys must be strings, numbers, or symbols
Reflect.defineProperty(target, 'value', {value: 1, writable: true});
Reflect.defineProperty(target, 'value', {writable: 'yes'}); // ERROR

class Constructed {
  value: number;
  constructor(value: number) {
    this.value = value;
  }
}
Reflect.construct<[number], Constructed>(Constructed, [1]) as Constructed;
Reflect.construct<[number], Constructed>(Constructed, [1]) as string; // ERROR
