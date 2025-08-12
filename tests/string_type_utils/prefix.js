// Basic usage
type DataProp = StringPrefix<'data-'>;

'data-' as DataProp; // OK
'data-x' as DataProp; // OK
'data' as DataProp; // ERROR
'x-data-x' as DataProp; // ERROR

// General `string` is not a prefixed string
{
  declare const x: string;
  x as DataProp; // ERROR
}

// All prefixed strings are `string`s
// But not some specific string.
{
  declare const x: DataProp;
  x as string; // OK
  x as 'data-'; // ERROR
}

// Use in a dictionary
type Dict = {+[DataProp]: mixed};
{
  const o = {
    'data-foo': 1,
    'data-bar': true,
  };
  const d = o as Dict; // OK
  d['data-baz']; // OK
  d[123]; // ERROR
  'data-baz' as $Keys<Dict>; // OK
  declare const x: DataProp;
  x as $Keys<Dict>; // OK
  x as $Keys<{['xxx']: mixed}>; // ERROR
}

// Can use string properties and methods
{
  declare const x: DataProp;
  x.length; // OK
  x.at(0); // OK
}

// Only string literal prefixes are allowed
type Err = StringPrefix<string>; // ERROR
declare function err<T: string>(StringPrefix<T>): void; // ERROR

// Refinements works
{
  declare const x: DataProp | number;
  if (typeof x === 'string') {
    x as DataProp; // OK
    x as empty; // ERROR
  } else {
    x as number; // OK
  }
}

// Prefix `data-foo-` is a subtype of prefix `data-`
type DataFooProp = StringPrefix<'data-foo-'>;
{
  declare const dataProp: DataProp;
  declare const dataFooProp: DataFooProp;

  dataFooProp as DataProp; // OK
  dataProp as DataFooProp; // ERROR
}

// Test type sig
declare export const data: DataProp;

// Type arg arity
type NoArgs = StringPrefix;
type ZeroTypeArgs = StringPrefix<>;
type TooManyTypeArgs = StringPrefix<'foo', 'bar', 'baz'>;

// With remainder (second type arg)
type Price = StringPrefix<'$', '1' | '2'>;
'$1' as Price; // OK
'$' as Price; // ERROR
'1' as Price; // ERROR
{
  declare const x: '$1' | '$2';
  x as Price; // OK
}
{
  declare const x: Price;
  x as string; // OK
  x as '$1' | '$2'; // ERROR: we don't support this
}
{
  declare const x: StringPrefix<'foo'>;
  x as StringPrefix<'foo', string>; // OK
  x as StringPrefix<'foo', 'xxx'>; // ERROR
}
{
  declare const x: StringPrefix<'foo', 'bar'>;
  x as string; // OK
  x as StringPrefix<'foo'>; // OK
  x as StringPrefix<'foo', string>; // OK
  x as StringPrefix<'foo', 'bar'>; // OK
  x as StringPrefix<'foo', 'xxx'>; // ERROR

  x as StringPrefix<'f'>; // OK
  x as StringPrefix<'f', 'bar'>; // ERROR
}
type RemainderTypeErr = StringPrefix<'foo', 1>; // ERROR
{
  declare function stripDollar<X: string>(x: StringPrefix<'$', X>): X;
  const x =  stripDollar("$2");
  x as "2"; // OK
  x as "3"; // ERROR
}

type SpreadOverOptionalProperties = StringPrefix<'foo'>;
{
  const obj: {[SpreadOverOptionalProperties]: number} = {foo: 1};
  const objCopy: {[SpreadOverOptionalProperties]: number, bar?: number} = {...obj}; // OK
  const readonlyObj: $ReadOnly<{[SpreadOverOptionalProperties]: number, bar?: number}> = obj; // OK
  const noCopyReadWrite: {[SpreadOverOptionalProperties]: number, bar?: number} = obj; // ERROR
  const validKeyInvalidValue: {[SpreadOverOptionalProperties]: number, +foobar?: string} = obj; // ERROR
  
}
