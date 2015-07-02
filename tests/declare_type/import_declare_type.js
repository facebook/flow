/**
 * @flow
 */

////////////////////////////////////////////////////////////
// == Import Declared Type Alias From Declared Module == //
//////////////////////////////////////////////////////////
import type {baz} from "ModuleAliasFoo";
import {foo} from "ModuleAliasFoo";
var k1: baz = 42;
var k2: baz = "shab"; // Error: string to int
var k3: toz = foo(k1); // Error: unknown identifier toz

import type {toz} from "ModuleAliasFoo";
var k4: toz = foo(k1); // works
