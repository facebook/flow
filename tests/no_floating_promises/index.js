// @flow strict

async function foo() {
  return 1;
}
declare var x: Promise<number>;
declare var z: { +x: Promise<boolean> };

foo().then(() => {}, () => {});

foo().catch(() => {});

foo().then(() => {});

foo();

x;

z.x;