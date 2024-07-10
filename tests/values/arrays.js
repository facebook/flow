type Arr = $Values<Array<number>>;
{
  1 as Arr; // OK
  declare const x: Arr;
  x as number; // OK
  x as empty; // ERROR
}

type ROArr = $Values<$ReadOnlyArray<number>>;
{
  1 as ROArr; // OK
  declare const x: ROArr;
  x as number; // OK
  x as empty; // ERROR
}

type Tup = $Values<['a', 'b']>;
{
  'a' as Tup; // OK
  'b' as Tup; // OK
  declare const x: Tup;
  x as 'a' | 'b'; // OK
  x as empty; // ERROR
}
