(`foo`: string); // OK
(`bar`: 'bar'); // OK
(`baz`: number); // ERROR

`foo ${123} bar`; // OK: number can be appended to string
`foo ${{bar: 123}} baz`; // ERROR: object can't be appended

{
  declare const x: string;
  `foo ${x}`; // OK
  `${x} bar`; // OK
  `foo ${'bar'} ${x}`; // OK
}
{
  declare const x: number;
  `foo ${x}`; // OK
  `${x} bar`; // OK
  `foo ${'bar'} ${x}`; // OK
}
{
  declare const x: boolean;
  `foo ${x}`; // ERROR
  `${x} bar`; // ERROR
  `foo ${'bar'} ${x}`; // ERROR
}
{
  declare const x: mixed;
  `foo ${x}`; // ERROR
  `${x} bar`; // ERROR
  `foo ${'bar'} ${x}`; // ERROR
}
