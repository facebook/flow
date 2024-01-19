import declare_module_exports from "declare_module_exports";
(declare_module_exports: number);
(declare_module_exports: string); // Error: number ~> string

// Error: Has no named export "str"!
import {str} from "declare_m_e_with_other_value_declares";

import type {str2} from "declare_m_e_with_other_type_declares";
("asdf": str2);
(42: str2); // Error: number ~> string

/**
 * `declare var exports` is deprecated, so we have a grace period where both
 * syntaxes will work.
 */

import declare_var_exports from "declare_var_exports";
(declare_var_exports.exports: number); // ok
(declare_var_exports.exports: string); // Error: number ~> string

import declare_m_e_with_declare_var_e from "declare_m_e_with_declare_var_e";
(declare_m_e_with_declare_var_e: number);
(declare_m_e_with_declare_var_e: string); // Error: number ~> string

import { foo } from "declare_overloaded_function";
(foo(0): number);
(foo(0): string); // Error: number ~> string
(foo(""): string);
(foo(""): number); // Error: string ~> number

import type {BT} from "B";
(42: BT); // ok
("str": BT); // Error: string ~> number

import type BDefault from "B";
import BDefaultValue from "B";
(new BDefaultValue(): BDefault); // ok
(42: BDefault); // Error: number ~> Def

// import between libdef files
import type {CT} from "C";
import type {DT} from "D"
{
  declare var cVal: CT;
  const dVal = {C: cVal};
  (cVal: CT); // ok
  (cVal.D: DT); // ok
  (dVal: DT); // ok
  (dVal.C: CT); // ok
}

import E from "E";
(new E(): BDefault); // ok
(new E(): empty); // error

/**
 * TODO: At the moment it isn't possible to import a non-libdef module from a
 *       libdef. There's no good reason to ban this, it's just a limitation of
 *       the way Flow handles libdefs at the moment. We should fix this test
 *       to pass at some point.
 */
// import {T} from "DependsOnRealModule";
// (42: T);
// ("str": T);
