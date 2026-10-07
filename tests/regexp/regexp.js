var patt=/Hello/g
var match:number = patt.test("Hello world!");

declare const regExp: RegExp;
regExp[Symbol.matchAll] as (str: string) => Iterator<RegExp$matchResult>; // error: the method's `this` is `RegExp`
regExp[Symbol.match] as (str: string) => Iterator<RegExp$matchResult>;

var escaped: string = RegExp.escape("hello[world]");
