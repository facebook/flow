(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * Information about the enclosing syntactic context of an expression. Each
 * one of these variants corresponds to the context of `C` in the expression
 * or statement shown in comment above it.
 *)
type enclosing_context =
  | NoContext
  (* switch(C){...} *)
  | SwitchTestContext of {
      case_test_loc: ALoc.t;
      switch_discriminant_loc: ALoc.t;
    }
  (*
   * if(C){}
   * while(C){}
   * C?e1:e2
   * invariant(C)
   *)
  | OtherTestContext
  (*
   * o[C]
   * o[C]=e
   * {[C]:e}
   * o[C]()
   *)
  | IndexContext
  (* <C /> *)
  | JsxTitleNameContext
  (* <Foo bar={C}>{C}</Foo> *)
  | JsxAttrOrChildrenContext
