import Foo = require("./export_assignment");
import type FooType = require("./export_assignment");

Foo.x as number; // OK
Foo.y as string; // OK
Foo.x as empty; // ERROR
