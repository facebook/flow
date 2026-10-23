declare const constructor: AsyncGeneratorFunctionConstructor;
const factory: AsyncGeneratorFunction = constructor('yield 1');
new constructor('yield 1') as AsyncGeneratorFunction;
constructor.name as string;
constructor.length as number;
constructor(1); // ERROR

const generator: AsyncGenerator = factory();
new factory() as AsyncGenerator;
factory.prototype as AsyncGenerator;
factory.name as string;
factory.length as number;
generator.next();
generator.next('anything');
generator.return('anything');
generator[Symbol.asyncIterator]() as AsyncGenerator;
generator[Symbol.asyncDispose]();
generator.next().then(result => {
  if (!result.done) {
    result.value as string; // ERROR: unknown default yield
  }
});

declare const typed: AsyncGenerator<number, string, boolean>;
typed.next(false);
typed.return('done');
typed.return(Promise.resolve<string>('done'));
typed.return(); // ERROR
typed.return(1); // ERROR
typed.throw(); // OK: any accepts void
typed.throw(new Error());
