import {x as any_x, y as any_y} from "any";

any_x as empty; any_y as empty; // ok. They are any typed.

import Any1 from "any";
const Any2 = require("any");
Any1 as empty; // ok. Any1 is any typed.
Any2 as empty; // ok. Any2 is any typed.

import * as Any3 from "any";
Any3 as number; // error
Any3 as {+[key: string]: any}; // ok
Any3.foo as string; Any3.foo as empty; // ok: Any3.foo is any

import {x as obj_x, y as obj_y} from "object";
obj_x as empty; obj_y as empty; // ok. They are any typed.

import Any4 from "object";
const Any5 = require("object");
Any4 as empty; // ok. Any4 is any typed.
Any5 as empty; // ok. Any5 is any typed.

import * as Any6 from "object";
Any6 as number; // error
Any6 as {+[key: string]: any}; // ok
Any6.foo as string; Any3.foo as empty; // ok: Any6.foo is any

import {x as str_x, y as str_y} from "string"; // error
str_x as string; // No error. Missing import is typed as any.
