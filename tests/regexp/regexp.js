var patt=/Hello/g
var match:number = patt.test("Hello world!");

declare var regExp: RegExp;
regExp[Symbol.matchAll] as (str: string) => Iterator<RegExp$matchResult>;
regExp[Symbol.match] as (str: string) => Iterator<RegExp$matchResult>;

var escaped: string = RegExp.escape("hello[world]");
