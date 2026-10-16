`foo` as string; // OK
`bar` as 'bar'; // OK
`baz` as number; // ERROR

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
  declare const x: unknown;
  `foo ${x}`; // ERROR
  `${x} bar`; // ERROR
  `foo ${'bar'} ${x}`; // ERROR
}
{
  declare opaque type ClientNum<T> super T extends T;
  declare const n: ClientNum<number>;
  `foo ${n}`; // OK - opaque number unwrapped via operator concretization
  `${n} bar`; // OK
  `foo ${'bar'} ${n}`; // OK
}
{
  declare opaque type ClientStr<T> super T extends T;
  declare const s: ClientStr<string>;
  `foo ${s}`; // OK - opaque string
  `${s} bar`; // OK
  `foo ${'bar'} ${s}`; // OK
}
{
  declare opaque type ClientBool<T> super T extends T;
  declare const b: ClientBool<boolean>;
  `foo ${b}`; // ERROR - opaque boolean still rejected
  `${b} bar`; // ERROR
  `foo ${'bar'} ${b}`; // ERROR
}
{
  declare opaque type ClientObj<T> super T extends T;
  declare const o: ClientObj<{}>;
  `foo ${o}`; // ERROR - opaque object still rejected
  `${o} bar`; // ERROR
  `foo ${'bar'} ${o}`; // ERROR
}
{
  declare opaque type OStr: string;
  declare opaque type ONum: number;
  declare opaque type OBool: boolean;
  declare opaque type OObj: {};
  declare const ostr: OStr;
  declare const onum: ONum;
  declare const obool: OBool;
  declare const oobj: OObj;
  `foo ${ostr}`; // OK - plain opaque string/number also unwrapped
  `${onum} bar`; // OK
  `foo ${obool}`; // ERROR - plain opaque boolean still rejected
  `foo ${oobj}`; // ERROR - plain opaque object still rejected
}
