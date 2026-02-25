declare const o: {|
  0: 'a',
|};

{
  const {0: x} = o;
  x as 'a'; // OK
  x as empty; // ERROR
}

// Number literal key with rest: rest should exclude property 0
declare const o2: {|
  0: 'a',
  1: 'b',
|};

{
  const {0: x, ...rest} = o2;
  x as 'a'; // OK
  rest as {| 1: 'b' |}; // OK
  rest as empty; // ERROR
}
