// Even through these things have conformance errors, outside of common interface conformance checks,
// we will not perform structural checks again.

import {BadDeepHierarchy as ClassIOS} from './classes.ios';
import {BadDeepHierarchy as ClassCommon} from './classes';
declare const instanceIOS: ClassIOS
instanceIOS as ClassCommon; // ok

import type {OpaqueType3 as OpaqueIOS} from './opaque_type.ios';
import type {OpaqueType3 as OpaqueCommon} from './opaque_type';
declare const opaque: OpaqueIOS
opaque as OpaqueCommon; // ok

import type {BarBad1 as EnumIOS} from './enums.ios';
import type {BarBad1 as EnumCommon} from './enums';
declare const e: EnumIOS
e as EnumCommon; // ok
