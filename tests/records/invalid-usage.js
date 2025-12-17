// Object literal cannot be assigned to record type
{
  record R {
    a: number,
  }

  const x: R = {a: 1}; // ERROR: should suggest `R {...}`
  const o = {a: 1};
  o as R; // ERROR: should suggest `R {...}`
}
