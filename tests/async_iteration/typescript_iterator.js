declare const next: (...[value]: [] | [boolean]) => Promise<IteratorResult<number, string>>;
const iterator: AsyncIterator<number, string, boolean> = {next};
iterator.next();
iterator.next(true);
iterator.next(1); // ERROR
iterator.next(true, false); // ERROR
iterator as $AsyncIterator<number, string, boolean>;
iterator as AsyncIterable<number, string, boolean>; // ERROR: missing async iterator method

const returnMethod = iterator.return;
if (returnMethod) {
  returnMethod();
  returnMethod('done');
  returnMethod(Promise.resolve('done'));
  returnMethod(1); // ERROR
}
const throwMethod = iterator.throw;
if (throwMethod) {
  throwMethod();
  throwMethod(new Error());
}

declare const iterable: AsyncIterable<number, string, boolean>;
iterable as $AsyncIterable<number, string, boolean>;
iterable[Symbol.asyncIterator]().next(true);

declare const iterableIterator: AsyncIterableIterator<number, string, boolean>;
iterableIterator[Symbol.asyncIterator]() as AsyncIterableIterator<number, string, boolean>;
iterableIterator.next(true);

declare const defaults: AsyncIterator<number>;
defaults.next('anything');
defaults.next().then(result => {
  if (!result.done) {
    result.value as number;
    result.value as string; // ERROR
  }
});

declare const objectDefaults: AsyncIteratorObject<number>;
objectDefaults[Symbol.asyncIterator]() as AsyncIteratorObject<number>;
objectDefaults.next().then(result => {
  if (result.done) {
    result.value as string; // ERROR: unknown default return
  }
});
