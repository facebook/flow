import {
  one as importedOne ,
  a as importedA,
  true_ as importedTrue,
  bigInt as importedBigInt,
} from "./export-primitive";

// Numeric - local
{
  1 as const as 1;
  1 as const as number;
  1 as const as 2; // error 1 ~> 2

  const localOne = 1 as const;
  type LocalOne = typeof localOne;
  1 as LocalOne; // okay
  2 as LocalOne; // error 2 ~> 1
  2 as number as LocalOne; // error number ~> 1
}

// Numeric - imported
{
  importedOne as 1; // okay
  importedOne as number; // okay
  type ImportedOne = typeof importedOne;
  1 as ImportedOne; // okay
  2 as ImportedOne; // error 2 ~> 1
  2 as number as ImportedOne; // error number ~> 1
}

// String - local
{
  "a" as const as "a";
  "a" as const as string;
  "a" as const as "b"; // error a ~> b

  const localA = "a" as const;
  type LocalA = typeof localA;
  "a" as LocalA; // okay
  "b" as LocalA; // error b ~> a
  "b" as string as LocalA; // error string ~> a
}

// String - imported
{
  importedA as "a"; // okay
  importedA as string; // okay
  type ImportedA = typeof importedA;
  "a" as ImportedA; // okay
  "b" as ImportedA; // error b ~> a
  "b" as string as ImportedA; // error string ~> a
}

// Boolean - local
{
  true as const as true;
  true as const as boolean;
  true as const as false; // error true ~> false

  const localTrue = true as const;
  type LocalTrue = typeof localTrue;
  true as LocalTrue; // okay
  false as LocalTrue; // error false ~> true
  false as boolean as LocalTrue; // error bool ~> true
}

// Boolean - imported
{
  importedTrue as true; // okay
  importedTrue as boolean; // okay
  type ImportedTrue = typeof importedTrue;
  true as ImportedTrue; // okay
  false as ImportedTrue; // error false ~> true
  false as boolean as ImportedTrue; // error bool ~> true
}

// BigInt - local
{
  1n as const as 1;
  1n as const as bigint;
  1n as const as 2n; // error 1n ~> 2n

  const localBigInt = 1n as const;
  type LocalBigInt = typeof localBigInt;
  1n as LocalBigInt; // okay
  2n as LocalBigInt; // error 2n ~> 1n
  2n as bigint as LocalBigInt; // error bigint ~> 1n
}

// BigInt - imported
{
  importedBigInt as 1n; // okay
  importedBigInt as bigint; // okay
  type ImportedBigInt = typeof importedBigInt;
  1n as ImportedBigInt; // okay
  2n as ImportedBigInt; // error 2n ~> 1n
  2n as bigint as ImportedBigInt; // error bigint ~> 1n
}
