/**
 * @flow
 */

////////////////////////////////////////////////////////////
// == Import Typeof Generic Function (Default Export) == //
////////////////////////////////////////////////////////////

import typeof genericFnT from './ExportDefault_GenericFunction';
import genericFn from './ExportDefault_GenericFunction';

// Bare use keeps the polymorphism, just like `typeof genericFn`
var k1: genericFnT = genericFn;
var k1n: number = k1(42);
var k1s: string = k1(42); // Error: number ~> string
var k2: typeof genericFn = genericFn;
k1 as typeof genericFn;
k2 as genericFnT;
// Explicit type arguments still instantiate
var k3: genericFnT<number> = genericFn;
var k3s: string = k3(42); // Error: number ~> string

//////////////////////////////////////////////////////////
// == Import Typeof Generic Function (Named Export) == //
//////////////////////////////////////////////////////////

import typeof {genericFn2 as genericFn2T} from './ExportNamed_GenericFunction';
import {genericFn2} from './ExportNamed_GenericFunction';

var l1: genericFn2T = genericFn2;
var l1n: number = l1(42);
var l1s: string = l1(42); // Error: number ~> string

import {
  typeof genericFn2 as genericFn2U,
  genericFn2 as genericFn2Impl,
} from './ExportNamed_GenericFunction';

var l2: genericFn2U = genericFn2Impl;
var l2s: string = l2(42); // Error: number ~> string

/////////////////////////////////////////////////////////////
// == Import Typeof Generic Component (Default Export) == //
/////////////////////////////////////////////////////////////

import typeof GenericComponentT from './ExportDefault_GenericComponent';
import GenericComponent from './ExportDefault_GenericComponent';

// Bare use behaves exactly like `typeof GenericComponent`
var m1: GenericComponentT = GenericComponent;
var m2: typeof GenericComponent = GenericComponent;
m1 as typeof GenericComponent;
m2 as GenericComponentT;
// Explicit type arguments still instantiate
var m3: GenericComponentT<unknown> = GenericComponent;

///////////////////////////////////////////////////////////
// == Import Typeof Generic Component (Named Export) == //
///////////////////////////////////////////////////////////

import typeof {Poly as PolyT} from './ExportNamed_GenericComponent';
import {Poly} from './ExportNamed_GenericComponent';

// Bare use behaves exactly like `typeof Poly`
var n1: PolyT = Poly;
var n2: typeof Poly = Poly;
n1 as typeof Poly;
n2 as PolyT;
