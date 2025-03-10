{
  declare const x: mixed;

  if (Number.isFinite(x)) {
    x as number; // OK
  }

  if (Number.isInteger(x)) {
    x as number; // OK
  }

  if (Number.isNaN(x)) {
    x as number; // OK
  }

  if (Number.isSafeInteger(x)) {
    x as number; // OK
  }
}

{
  declare const x: null | number;

  if (Number.isFinite(x)) {
    x as number; // OK
  }

  if (Number.isInteger(x)) {
    x as number; // OK
  }

  if (Number.isNaN(x)) {
    x as number; // OK
  }

  if (Number.isSafeInteger(x)) {
    x as number; // OK
  }
}

{
  declare const x: {foo?: number};

  if (Number.isFinite(x.foo)) {
    x.foo as number; // OK
  }

  if (Number.isInteger(x.foo)) {
    x.foo as number; // OK
  }

  if (Number.isNaN(x.foo)) {
    x.foo as number; // OK
  }

  if (Number.isSafeInteger(x.foo)) {
    x.foo as number; // OK
  }
}
