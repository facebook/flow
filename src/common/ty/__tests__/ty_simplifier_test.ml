(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ty
open Ty_utils

module UnionSimplification = struct
  let tests =
    [
      (*
       * {f: number} | {f: number}
       * ~>
       * {f: number}
       *)
      ( "simplify_union_obj" >:: fun ctxt ->
        let t_in =
          Ty.Union
            ( false,
              Ty.Obj
                {
                  Ty.obj_kind = Ty.InexactObj;
                  obj_def_loc = None;
                  obj_literal = None;
                  obj_props =
                    [
                      Ty.NamedProp
                        {
                          name = Reason.OrdinaryName "f";
                          prop = Ty.Field { t = Ty.Num; polarity = Ty.Neutral; optional = false };
                          inherited = false;
                          source = Ty.Other;
                          def_locs = [];
                        };
                    ];
                },
              Ty.Obj
                {
                  Ty.obj_kind = Ty.InexactObj;
                  obj_def_loc = None;
                  obj_literal = None;
                  obj_props =
                    [
                      Ty.NamedProp
                        {
                          name = Reason.OrdinaryName "f";
                          prop = Ty.Field { t = Ty.Num; polarity = Ty.Neutral; optional = false };
                          inherited = false;
                          source = Ty.Other;
                          def_locs = [];
                        };
                    ];
                },
              []
            )
        in
        let t_out = Ty_utils.simplify_type ~merge_kinds:true ~sort:false t_in in
        let t_exp =
          Ty.Obj
            {
              Ty.obj_kind = Ty.InexactObj;
              obj_def_loc = None;
              obj_literal = None;
              obj_props =
                [
                  Ty.NamedProp
                    {
                      name = Reason.OrdinaryName "f";
                      prop = Ty.Field { t = Ty.Num; polarity = Ty.Neutral; optional = false };
                      inherited = false;
                      source = Ty.Other;
                      def_locs = [];
                    };
                ];
            }
        in
        assert_equal ~ctxt ~printer:Ty.show t_exp t_out
      );
      (*
       * {+f: number} | {-f: number}
       * ~>
       * {+f: number} | {-f: number}
       *)
      ( "simplify_union_obj" >:: fun ctxt ->
        let t_in =
          Ty.Union
            ( false,
              Ty.Obj
                {
                  Ty.obj_kind = Ty.InexactObj;
                  obj_def_loc = None;
                  obj_literal = None;
                  obj_props =
                    [
                      Ty.NamedProp
                        {
                          name = Reason.OrdinaryName "f";
                          prop = Ty.Field { t = Ty.Num; polarity = Ty.Positive; optional = false };
                          inherited = false;
                          source = Ty.Other;
                          def_locs = [];
                        };
                    ];
                },
              Ty.Obj
                {
                  Ty.obj_kind = Ty.InexactObj;
                  obj_def_loc = None;
                  obj_literal = None;
                  obj_props =
                    [
                      Ty.NamedProp
                        {
                          name = Reason.OrdinaryName "f";
                          prop = Ty.Field { t = Ty.Num; polarity = Ty.Negative; optional = false };
                          inherited = false;
                          source = Ty.Other;
                          def_locs = [];
                        };
                    ];
                },
              []
            )
        in
        let t_out = Ty_utils.simplify_type ~merge_kinds:true ~sort:false t_in in
        let t_exp = t_in in
        assert_equal ~ctxt ~printer:Ty.show t_exp t_out
      );
    ]
end

module BotAndTopSimplification = struct
  let tests =
    [
      (* When merge_kinds is true, all kinds of `empty` are equivalent, even when
       * nested under a type constructor.
       *
       * {f: empty} | {f: empty'}
       * ~> (merge_kinds:true)
       * {f: empty'}
       *)
      ( "simplify_union_obj_empty_insensitive" >:: fun ctxt ->
        let t_in =
          Ty.Union
            ( false,
              Ty.Obj
                {
                  Ty.obj_kind = Ty.InexactObj;
                  obj_def_loc = None;
                  obj_literal = None;
                  obj_props =
                    [
                      Ty.NamedProp
                        {
                          name = Reason.OrdinaryName "f";
                          prop =
                            Ty.Field
                              { t = Ty.Bot Ty.EmptyType; polarity = Ty.Neutral; optional = false };
                          inherited = false;
                          source = Ty.Other;
                          def_locs = [];
                        };
                    ];
                },
              Ty.Obj
                {
                  Ty.obj_kind = Ty.InexactObj;
                  obj_def_loc = None;
                  obj_literal = None;
                  obj_props =
                    [
                      Ty.NamedProp
                        {
                          name = Reason.OrdinaryName "f";
                          prop =
                            Ty.Field
                              { t = Ty.Bot Ty.EmptyType; polarity = Ty.Neutral; optional = false };
                          inherited = false;
                          source = Ty.Other;
                          def_locs = [];
                        };
                    ];
                },
              []
            )
        in
        let t_out = Ty_utils.simplify_type ~merge_kinds:true ~sort:false t_in in
        let t_exp =
          Ty.Obj
            {
              Ty.obj_kind = Ty.InexactObj;
              obj_def_loc = None;
              obj_literal = None;
              obj_props =
                [
                  Ty.NamedProp
                    {
                      name = Reason.OrdinaryName "f";
                      prop =
                        Ty.Field
                          { t = Ty.Bot Ty.EmptyType; polarity = Ty.Neutral; optional = false };
                      inherited = false;
                      source = Ty.Other;
                      def_locs = [];
                    };
                ];
            }
        in
        assert_equal ~ctxt ~printer:Ty.show t_exp t_out
      );
    ]
end

module AnySimplification = struct
  open Ty

  (* When merge_kinds is false, we preserve the different kinds of any.
   *
   * any | (any' & (any & any'))
   * ~>
   * any | (any' & (any & any'))
   *)
  let tests =
    [
      ( "merge_any_kinds_sensitive" >:: fun ctxt ->
        let t_in =
          Union
            ( false,
              Any (Unsound BoundFunctionThis),
              Inter
                ( Any (Annotated ALoc.none),
                  Union (false, Any (Unsound BoundFunctionThis), Ty.Any (Annotated ALoc.none), []),
                  []
                ),
              []
            )
        in
        let t_out = Ty_utils.simplify_type ~merge_kinds:false t_in in
        let t_exp = t_in in
        assert_equal ~ctxt ~printer:Ty.show t_exp t_out
      );
      (* When merge_kinds is true, all kinds of any are considered equal and so
       * are merged when appearing in unions or intersections.
       *
       * any | (any' & (any & any'))
       * ~>
       * any
       *
       * The output could also be any'. The kind of the resulting any type when
       * merge_kinds is true, is not specified.
       *)
      ( "merge_any_kinds_insensitive" >:: fun ctxt ->
        let t_in =
          Union
            ( false,
              Any (Unsound BoundFunctionThis),
              Inter
                ( Any (Annotated ALoc.none),
                  Union (false, Any (Unsound BoundFunctionThis), Ty.Any (Annotated ALoc.none), []),
                  []
                ),
              []
            )
        in
        let t_out = Ty_utils.simplify_type ~merge_kinds:true t_in in
        let t_exp = Any (Unsound BoundFunctionThis) in
        assert_equal ~ctxt ~printer:Ty.show t_exp t_out
      );
    ]
end

module Sorting = struct
  let simplify_base = simplify_type ~merge_kinds:false ~sort:false

  let simplify_sort = simplify_type ~merge_kinds:false ~sort:true

  let t0 = Union (false, Any (Annotated ALoc.none), Num, [NumLit "42"])

  let t1 = Union (false, NumLit "1", NumLit "2", [NumLit "42"])

  let t2 = Union (false, NumLit "2", t0, [t1])

  let t3 = Union (false, t0, t1, [t2])

  let t4 = Union (false, t3, t2, [t1; t0])

  let t5 = Union (false, t0, t1, [t2; t3; t4])

  let t6 = Union (false, t3, t2, [t4; t0; t1; t5])

  let t6_sorted =
    Union (false, Any (Annotated ALoc.none), NumLit "1", [NumLit "2"; NumLit "42"; Num])

  let tests =
    [
      ( "idempotence" >:: fun ctxt ->
        assert_equal
          ~ctxt
          ~printer:(Ty_printer.string_of_t ~exact_by_default:true ~ts_syntax:false)
          (simplify_base t0)
          (simplify_base (simplify_base t0));
        assert_equal
          ~ctxt
          ~printer:(Ty_printer.string_of_t ~exact_by_default:true ~ts_syntax:false)
          (simplify_base t6)
          (simplify_base (simplify_base (simplify_base t6)));
        assert_equal
          ~ctxt
          ~printer:(Ty_printer.string_of_t ~exact_by_default:true ~ts_syntax:false)
          (simplify_sort t4)
          (simplify_sort (simplify_sort t4));
        assert_equal
          ~ctxt
          ~printer:(Ty_printer.string_of_t ~exact_by_default:true ~ts_syntax:false)
          (simplify_sort t6)
          (simplify_sort (simplify_sort (simplify_sort t6)))
      );
      ( "sorting" >:: fun ctxt ->
        assert_equal
          ~ctxt
          ~printer:(Ty_printer.string_of_t ~exact_by_default:true ~ts_syntax:false)
          t6_sorted
          (simplify_sort t6)
      );
      ( "union/intersection" >:: fun ctxt ->
        let t_in =
          Inter
            ( Union
                ( false,
                  Void,
                  Inter (Void, Any (Annotated ALoc.none), [NumLit "1"]),
                  [Inter (NumLit "1", Any (Annotated ALoc.none), [Void])]
                ),
              Union (false, Inter (Any (Annotated ALoc.none), Void, [NumLit "1"]), Void, []),
              []
            )
        in
        let t_out = simplify_sort t_in in
        let t_exp =
          Union (false, Void, Inter (Any (Annotated ALoc.none), Void, [NumLit "1"]), [])
        in
        assert_equal ~ctxt ~printer:Ty.show t_exp t_out
      );
    ]
end

let tests =
  "ty_simplifier"
  >::: UnionSimplification.tests
       @ BotAndTopSimplification.tests
       @ AnySimplification.tests
       @ Sorting.tests
