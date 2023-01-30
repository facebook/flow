import {x as any_x, y as any_y} from "any";

(any_x: empty); (any_y: empty); // ok. They are any typed.

import Any1 from "any";
const Any2 = require("any");
(Any1: empty); // ok. Any1 is any typed.
(Any2: empty); // ok. Any2 is any typed.

import * as Any3 from "any";
(Any3: number); // error
(Any3: {[key: string]: any}); // ok
(Any3.foo: string); (Any3.foo: empty); // ok: Any3.foo is any

import {x as obj_x, y as obj_y} from "object";
(obj_x: empty); (obj_y: empty); // ok. They are any typed.

import Any4 from "object";
const Any5 = require("object");
(Any4: empty); // ok. Any4 is any typed.
(Any5: empty); // ok. Any5 is any typed.

import * as Any6 from "object";
(Any6: number); // error
(Any6: {[key: string]: any}); // ok
(Any6.foo: string); (Any3.foo: empty); // ok: Any6.foo is any

import {x as str_x, y as str_y} from "string"; // error
(str_x: string); // No error. Missing import is typed as any.
