// Access with `number`
{
  declare const o: {foo: 1};
  declare const n: number;
  o[n]; // ERROR
}
