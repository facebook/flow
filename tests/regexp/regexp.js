var patt=/Hello/g
var match:number = patt.test("Hello world!");

declare const regExp: RegExp;
regExp[Symbol.matchAll] as (str: string) => Iterator<RegExp$matchResult>; // error: the method's `this` is `RegExp`
regExp[Symbol.match] as (str: string) => Iterator<RegExp$matchResult>;

var escaped: string = RegExp.escape("hello[world]");

// `RegExpStringIterator<T>` is declared without variance, so `T` is invariant.
const matches = "abc".matchAll(/b/g);
matches as Iterator<RegExpMatchArray | null>; // ok: `Iterator`'s element type is covariant
matches as RegExpStringIterator<RegExpMatchArray | null>; // error: invariant
matches as RegExpStringIterator<Array<string>>; // error: invariant, so `Array<string>` needs `index` and `input`
