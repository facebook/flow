/**
 * @flow
 */

////////////////////////////////////////////////////////////
// == Import Declared Type Alias From Declared Module == //
//////////////////////////////////////////////////////////
import type {baz} from 'ModuleAliasFoo';
import {foo} from 'ModuleAliasFoo';
var k1: baz = 42;
var k2: baz = 'shab'; // Error: string to int
var k3: toz = foo(k1); // works

import type {toz} from 'ModuleAliasFoo';
var k4: toz = foo(k1); // works

//////////////////////////////////////////////////////////
// == Declared Module with exports prop (issue 880) == //
////////////////////////////////////////////////////////

import blah from 'foo';
import type {Foo, Bar, Id} from 'foo';

blah(0, 0);

({toz: 3}) as Foo; // error : {toz : number} ~> string

3 as Bar; // error : number ~> A

'lol' as Id<number>; // error : string ~> number

///////////////////////////////////////////////////////
// == Permit unnecessary declare export of types == //
/////////////////////////////////////////////////////
import type {DeclareExportType, DeclareExportInterface} from './exported';
"3" as DeclareExportType; // error: string ~> number
({}) as DeclareExportInterface; // error: prop-missing
