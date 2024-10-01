export const n = 1 as const;
export const s = "a" as const;
export const b = true as const;
export const bi = 1n as const;
export const neg = -1 as const;
export const obj_neg = { f: -1 } as const;

function test_numeric() {
  n as 1;
  n as number;
  n as 2; // error 1 ~> 2

  1 as typeof n; // okay
  2 as typeof n; // error 2 ~> 1
  2 as number as typeof n; // error number ~> 1
}

function test_string() {
  s as "a";
  s as string;
  s as "b"; // error a ~> b

  "a" as typeof s; // okay
  "b" as typeof s; // error b ~> a
  "b" as string as typeof s; // error string ~> a
}

function test_boolean() {
  b as true;
  b as boolean;
  b as false; // error true ~> false

  true as typeof b; // okay
  false as typeof b; // error false ~> true
  false as boolean as typeof b; // error bool ~> true
}

function test_bigint() {
  bi as 1n;
  bi as bigint;
  bi as 2n; // error 1n ~> 2n

  1n as typeof bi; // okay
  2n as typeof bi; // error 2n ~> 1n
  2n as bigint as typeof bi; // error bigint ~> 1n
}

function test_numeric_negative() {
  neg as -1; // okay
  neg as 1; // error
  -1 as typeof neg; // okay
  1 as typeof neg; // error

  obj_neg.f as -1; // okay
  obj_neg.f as 1; // error
  -1 as typeof obj_neg.f; // okay
  1 as typeof obj_neg.f; // error
}
