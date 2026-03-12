type T = { [Symbol.iterator](): X };
type U = { [Symbol.asyncIterator](): X };
type V = { [Symbol.dispose](): void };
type W = { [Symbol.asyncDispose](): void };
type Y = { [Symbol.xxx](): string };
type Z = { +[Symbol.iterator]: () => X };
type A = { [Symbol.iterator]?(): X };
