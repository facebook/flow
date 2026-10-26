declare const readonlyTuple: Readonly<[Promise<number>, string]>;
const tupleResult = Promise.allSettled(readonlyTuple);
tupleResult as Promise<[PromiseSettledResult<number>, PromiseSettledResult<string>]>;
tupleResult as Promise<[PromiseSettledResult<string>, PromiseSettledResult<number>]>; // ERROR

declare const readonlyValues: ReadonlyArray<number | PromiseLike<number>>;
const arrayResult = Promise.allSettled(readonlyValues);
arrayResult as Promise<Array<PromiseSettledResult<number>>>;
arrayResult as Promise<Array<PromiseSettledResult<string>>>; // ERROR

const iterableResult = Promise.allSettled(new Set([Promise.resolve(1), Promise.resolve(2)]));
iterableResult as Promise<Array<PromiseSettledResult<number>>>;
iterableResult as Promise<Array<PromiseSettledResult<string>>>; // ERROR

const literalResult = Promise.allSettled([1, 'ok']);
literalResult as Promise<[PromiseSettledResult<number>, PromiseSettledResult<string>]>;

declare const nested: PromiseLike<PromiseLike<number>>;
const nestedResult = Promise.allSettled([nested]);
nestedResult as Promise<[PromiseSettledResult<number>]>;

declare const awaited: $AllSettledAwaited<number | PromiseLike<PromiseLike<number>>>;
awaited as number;
awaited as string; // ERROR

async function mutableTuple(): Promise<void> {
  const settled = await tupleResult;
  settled[0] = {status: 'fulfilled', value: 1};
  settled[1] = {status: 'fulfilled', value: 'ok'};
  settled[0] = {status: 'fulfilled', value: 'wrong'}; // ERROR
}

declare const fulfilled: PromiseFulfilledResult<number>;
fulfilled.value = 2;
fulfilled.value = 'wrong'; // ERROR
fulfilled.status = 'fulfilled';
fulfilled.status = 'rejected'; // ERROR

declare const rejected: PromiseRejectedResult;
rejected.status = 'rejected';
rejected.reason = new Error('rejected');

declare const settled: PromiseSettledResult<number>;
settled as PromiseSettledResult<number | string>; // ERROR
settled as $SettledPromiseResult<number>;
declare const legacy: $SettledPromiseResult<number>;
legacy as PromiseSettledResult<number>;
