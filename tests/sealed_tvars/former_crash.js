//@flow

declare class SearchableEntry<T> {
  constructor(d: T): void
}

function ouch<X>(d: X) {
  new SearchableEntry({ ...d });
}
