// number
{
  declare const one: 1;
  declare const two: 2;
  declare const x: 1 | 2;

  if (x === one) {
    x as 1; // OK
  } else if (x === two) {
    x as 2; // OK
  } else {
    x as empty; // OK
  }

  if (x === one) {
    x as 1; // OK
  } else {
    x as empty; // ERROR: `2` remains
  }

  if (x !== one) {
    x as 2; // OK
  } else {
    x as 1; // OK
    x as empty; // ERROR: it is `1`
  }
}

{
  declare const one: 1;
  declare const x: number;

  if (x === one) {
    x as 1; // OK
  } else {
    x as number; // OK
    x as empty; // ERROR: still `number`
  }

  if (x !== one) {
    x as number; // OK
  } else {
    x as 1; // OK
    x as empty; // ERROR: it is `1`
  }
}

// bigint
{
  declare const one: 1n;
  declare const two: 2n;
  declare const x: 1n | 2n;

  if (x === one) {
    x as 1n; // OK
  } else if (x === two) {
    x as 2n; // OK
  } else {
    x as empty; // OK
  }
}

// string
{
  declare const foo: 'foo';
  declare const bar: 'bar';
  declare const x: 'foo' | 'bar';

  if (x === foo) {
    x as 'foo'; // OK
  } else if (x === bar) {
    x as 'bar'; // OK
  } else {
    x as empty; // OK
  }
}

{
  declare const foo: 'foo';
  declare const x: string;

  if (x === foo) {
    x as 'foo'; // OK
  } else {
    x as string; // OK
    x as empty; // ERROR: still `string`
  }
}

// boolean
{
  declare const t: true;
  declare const f: false;
  declare const x: true | false;

  if (x === t) {
    x as true; // OK
  } else if (x === f) {
    x as false; // OK
  } else {
    x as empty; // OK
  }
}

// null
{
  declare const n: null;
  declare const x: null | 1;

  if (x === n) {
    x as null; // OK
  } else {
    x as 1; // OK
    x as empty; // ERROR: it is `1`
  }
}

// undefined
{
  declare const u: void;
  declare const x: void | 1;

  if (x === u) {
    x as void; // OK
  } else {
    x as 1; // OK
    x as empty; // ERROR: it is `1`
  }
}
