import Foo = require("./export_assignment");
export import Bar = Foo.x; // ERROR
declare const y: Bar; // ERROR: `Bar` cannot be resolved because the qualified name import above is unsupported
