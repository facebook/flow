declare const o: {|
  0: 'a',
|};

{
  const {0: x} = o;
  x as 'a'; // OK
  x as empty; // ERROR
}
