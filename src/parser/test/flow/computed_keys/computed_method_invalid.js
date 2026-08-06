type T = { [string[]](): X };
type U = { [void](): X };
type V = { [Foo<string>](): X };
type W = { [import('m').Y](): X };
