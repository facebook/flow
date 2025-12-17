// Object literal cannot be assigned to record type
{
  record R {
    a: number,
  }

  const x: R = {a: 1}; // ERROR: should suggest `R {...}`
  const o = {a: 1};
  o as R; // ERROR: should suggest `R {...}`
}

// New with object literal suggests record expression syntax
{
  record R {
    a: number,
  }

  new R({a: 1}); // ERROR: should suggest record expression
}
{
  record R<T> {
    a: T,
  }

  new R<number>({a: 1}); // ERROR: should suggest record expression
}
