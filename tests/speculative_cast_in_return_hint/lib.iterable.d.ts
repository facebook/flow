interface HeadersIterator<T> extends IteratorObject<T, BuiltinIteratorReturn, unknown> {
  [Symbol.iterator](): HeadersIterator<T>;
}

interface Headers {
  [Symbol.iterator](): HeadersIterator<[string, string]>;
  entries(): HeadersIterator<[string, string]>;
}
