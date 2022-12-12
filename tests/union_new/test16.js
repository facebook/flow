// @noflow

// annotations

type T = number | (() => string);
type Foo = T | (() => boolean);

type Bar = number | (() => string) | (() => boolean);

function foo(x: Foo) { }
foo(() => qux());

function bar(x: Bar) { }
bar(() => qux());

var x: boolean | string = false;
function qux() { return x; }
x = "";
