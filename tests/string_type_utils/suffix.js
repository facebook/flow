// Basic usage
type Percent = StringSuffix<'%'>;

'%' as Percent; // OK
'12%' as Percent; // OK
'' as Percent; // ERROR
'%6' as Percent; // ERROR

// General `string` is not a suffixed string
{
  declare const x: string;
  x as Percent; // ERROR
}

// All suffixed strings are `string`s
// But not some specific string.
{
  declare const x: Percent;
  x as string; // OK
  x as '%'; // ERROR
}

// Can use string properties and methods
{
  declare const x: Percent;
  x.length; // OK
  x.at(0); // OK
}

// Only string literal suffixes are allowed
type Err = StringSuffix<string>; // ERROR
declare function err<T: string>(StringSuffix<T>): void; // ERROR

// Refinements works
{
  declare const x: Percent | number;
  if (typeof x === 'string') {
    x as Percent; // OK
    x as empty; // ERROR
  } else {
    x as number; // OK
  }
}

// Suffix `foobar` is a subtype of suffix `bar`
type FooBar = StringSuffix<'foobar'>;
type Bar = StringSuffix<'bar'>;
{
  declare const foobar: FooBar;
  declare const bar: Bar;

  foobar as Bar; // OK
  bar as FooBar; // ERROR
}

// Test type sig
declare export const percent: Percent;

// Type arg arity
type NoArgs = StringSuffix;
type ZeroTypeArgs = StringSuffix<>;
type TooManyTypeArgs = StringSuffix<'foo', 'bar', 'baz'>;

// With remainder (second type arg)
type Exclamation = StringSuffix<'!', 'woo' | 'yay'>;
'yay!' as Exclamation; // OK
'!' as Exclamation; // ERROR
'yay' as Exclamation; // ERROR
{
  declare const x: 'woo!' | 'yay!';
  x as Exclamation; // OK
}
{
  declare const x: Exclamation;
  x as string; // OK
  x as 'woo!' | 'yay!'; // ERROR: we don't support this
}
{
  declare const x: StringSuffix<'!'>;
  x as StringSuffix<'!', string>; // OK
  x as StringSuffix<'!', 'xxx'>; // ERROR
}
{
  declare const x: StringSuffix<'foo', 'bar'>;
  x as string; // OK
  x as StringSuffix<'foo'>; // OK
  x as StringSuffix<'foo', string>; // OK
  x as StringSuffix<'foo', 'bar'>; // OK
  x as StringSuffix<'foo', 'xxx'>; // ERROR

  x as StringSuffix<'o'>; // OK
  x as StringSuffix<'o', 'bar'>; // ERROR
}
type RemainderTypeErr = StringSuffix<'foo', 1>; // ERROR
{
  declare function unexcite<X: string>(x: StringSuffix<'!', X>): X;
  const x =  unexcite("yay!");
  x as "yay"; // OK
  x as "yay!"; // ERROR
}
