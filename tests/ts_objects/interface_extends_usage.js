// @flow
// .js consumer of a .ts-declared interface whose super was relaxed in .ts.
// Using the imported interface from .js works -- the SuperT check ran at
// the .ts declaration site, not here.

import type {Ext} from "./interface_extends_lib";

declare const x: Ext;
x.a as number; // OK
x.c as boolean; // OK
