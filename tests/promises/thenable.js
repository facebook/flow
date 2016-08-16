// @flow

declare class Thenable<+R> {
  then<U>(
    onFulfill?: (value: R) => $Thenable<U> | U,
    onReject?: (error: any) => $Thenable<U> | U
  ): Thenable<U>;
}

declare function createThenable<T>(val: T): Thenable<T>;

const r0: $Thenable<number> = createThenable(1);
const r1: $Thenable<number> = Promise.resolve(1);
const r2: Promise<number> = Promise.resolve(createThenable(1));
const r3: Promise<number> = new Promise(resolve => {
  resolve(createThenable(1));
});

const r4: Promise<number> = Promise.resolve().then(() => createThenable(1));
const r5: Promise<number> = Promise.resolve().then(() => createThenable(1), () => createThenable(1));

const r6: Promise<number> = Promise.resolve().catch(() => createThenable(1));

const r7: Promise<number> = Promise.race([createThenable(1), createThenable(1)]);

async function test(val: Thenable<number>) {
  const r: number = await val;
}
