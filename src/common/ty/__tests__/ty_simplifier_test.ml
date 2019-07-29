(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

open Ty
open Ty_utils

module Q1 = struct
  let is_bot = BotSensitiveQueries.is_bot
  let is_top = BotSensitiveQueries.is_top
  let compare x y = Pervasives.compare x y
  let sort_types = false
end

module S1 = Simplifier (Q1)

module Q2 = struct
  let is_bot = BotSensitiveQueries.is_bot
  let is_top = BotSensitiveQueries.is_top
  let c = object(_)
    inherit [_] comparator_ty as super
    method! tag_of_t () = function
      | Any _ -> 0
      | Top -> 1
      | Bot _ -> 2
      | Void -> 3
      | Null -> 4
      | BoolLit _ -> 5
      | NumLit _ -> 6
      | StrLit _ -> 7
      | Bool _ -> 8
      | Num _ -> 9
      | Str _ -> 10
      | TVar _ -> 11
      | Bound _ -> 12
      | Generic _ -> 13
      | t -> super#tag_of_t () t
  end
  let compare = c#compare ()
  let sort_types = true
end

module S2 = Simplifier (Q2)

let t0 = Union (Any Implicit, Num None, [NumLit "42"])
let t1 = Union (NumLit "1", NumLit "2", [NumLit "42"])
let t2 = Union (NumLit "2", t0, [t1])
let t3 = Union (t0, t1, [t2])
let t4 = Union (t3, t2, [t1; t0])
let t5 = Union (t0, t1, [t2; t3; t4])
let t6 = Union (t3, t2, [t4; t0; t1; t5])
let t6_sorted = Union (Any Implicit, NumLit "1", [NumLit "2"; NumLit "42"; Num None])

let tests = "ty_simplifier" >::: [
  (*
   * {f: number} | {f: number}
   * ~>
   * {f: number}
   *)
  "simplify_union_obj" >:: begin fun ctxt ->
    let t_in = Ty.Union (
      (Ty.Obj
        { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
          obj_props =
          [(Ty.NamedProp ("f",
              (Ty.Field ((Ty.Num None),
                 { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
              ))
            ]
          }),
      (Ty.Obj
        { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
          obj_props =
          [(Ty.NamedProp ("f",
              (Ty.Field ((Ty.Num None),
                 { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
              ))
            ]
          }),
      []) in
    let t_out = Ty_utils.simplify_type ~simplify_empty:true t_in in
    let t_exp = Ty.Obj {
      Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
      obj_props = [
        Ty.NamedProp ("f", (Ty.Field ((Ty.Num None),
          { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
        )
      ]
    } in
    assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
  end;

  (*
   * {+f: number} | {-f: number}
   * ~>
   * {+f: number} | {-f: number}
   *)
  "simplify_union_obj" >:: begin fun ctxt ->
    let t_in = Ty.Union (
      (Ty.Obj
        { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
          obj_props =
          [(Ty.NamedProp ("f",
              (Ty.Field ((Ty.Num None),
                 { Ty.fld_polarity = Ty.Positive; fld_optional = false }))
              ))
            ]
          }),
      (Ty.Obj
        { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
          obj_props =
          [(Ty.NamedProp ("f",
              (Ty.Field ((Ty.Num None),
                 { Ty.fld_polarity = Ty.Negative; fld_optional = false }))
              ))
            ]
          }),
      []) in
    let t_out = Ty_utils.simplify_type ~simplify_empty:true t_in in
    let t_exp = t_in in
    assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
  end;


  (* When simplify_empty is true, all kinds of `empty` are equivalent, even when
   * nested under a type constructor.
   *
   * {f: empty} | {f: empty'}
   * ~> (simplify_empty:true)
   * {f: empty'}
   *)
  "simplify_union_obj_empty_insensitive" >:: begin fun ctxt ->
    let t_in = Ty.Union (
      (Ty.Obj
        { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
          obj_props =
          [(Ty.NamedProp ("f",
              (Ty.Field ((Ty.Bot Ty.EmptyType),
                 { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
              ))
            ]
          }),
      (Ty.Obj
        { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
          obj_props =
          [(Ty.NamedProp ("f",
              (Ty.Field ((Ty.Bot Ty.EmptyMatchingPropT),
                 { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
              ))
            ]
          }),
      []) in
    let t_out = Ty_utils.simplify_type ~simplify_empty:true t_in in
    let t_exp = Ty.Obj
      { Ty.obj_exact = false; obj_frozen = false; obj_literal = false;
        obj_props =
        [(Ty.NamedProp ("f",
            (Ty.Field ((Ty.Bot Ty.EmptyType),
               { Ty.fld_polarity = Ty.Neutral; fld_optional = false }))
            ))]
      } in
    assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
  end;

  (* This tests the conversion `mixed & T -> T` and that `empty' | T` remains
   * as is when:
   * - `empty'` is not the empty type due to
   *   + an annotation, or
   *   + a tvar with no lower and no upper bounds
   * - simplify_empty is false
   *
   * mixed & (empty' | (mixed & (empty'' | number)))
   * ~> (simplify_empty:false)
   * empty' | empty'' | number
   *)
  "simplify_empty_sensitive" >:: begin fun ctxt ->
    let t_in = Ty.Inter (Ty.Top,
      Ty.Union (Ty.Bot (Ty.EmptyTypeDestructorTriggerT ALoc.none),
        Ty.Inter (Ty.Top,
           Ty.Union (Ty.Bot (Ty.NoLowerWithUpper
             (Ty.SomeUnknownUpper "blah")
            ), Ty.Num None, []), []),
        []),
      []) in
    let t_out = Ty_utils.simplify_type ~simplify_empty:false t_in in
    let t_exp = Ty.Union (Ty.Bot (Ty.EmptyTypeDestructorTriggerT ALoc.none),
      Ty.Bot (Ty.NoLowerWithUpper (Ty.SomeUnknownUpper "blah")),
      [Ty.Num None]
    ) in
    assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
  end;

  (* This tests the conversion `mixed & T -> T` and `empty' | T -> T` when
   * simplify_empty is true.
   *
   * mixed & (empty' | (mixed & (empty'' | number)))
   * ~>
   * number
   *)
  "simplify_empty_insensitive" >:: begin fun ctxt ->
    let t_in = Ty.Inter (Ty.Top,
      Ty.Union (Ty.Bot (Ty.EmptyTypeDestructorTriggerT ALoc.none),
        Ty.Inter (Ty.Top,
           Ty.Union (Ty.Bot (Ty.NoLowerWithUpper
             (Ty.SomeUnknownUpper "blah")
            ), Ty.Num None, []), []),
        []),
      []) in
    let t_out = Ty_utils.simplify_type ~simplify_empty:true t_in in
    let t_exp = Ty.Num None in
    assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
  end;
  "idempotence" >:: begin fun ctxt ->
    assert_equal ~ctxt ~printer:Ty_printer.string_of_t (S1.run t0) (S1.run (S1.run t0));
    assert_equal ~ctxt ~printer:Ty_printer.string_of_t (S1.run t6) (S1.run (S1.run (S1.run t6)));
    assert_equal ~ctxt ~printer:Ty_printer.string_of_t (S2.run t4) (S2.run (S2.run t4));
    assert_equal ~ctxt ~printer:Ty_printer.string_of_t (S2.run t6) (S2.run (S2.run (S2.run t6)));
  end;

  "sorting" >:: begin fun ctxt ->
    assert_equal ~ctxt ~printer:Ty_printer.string_of_t t6_sorted  (S2.run t6)
  end;

  "union/intersection" >:: begin fun ctxt ->
    let t_in = Inter (Union (Void, Inter (Void, Any Implicit, [NumLit "1"]),
                              [Inter (NumLit "1", Any Implicit, [Void])]),
                      Union (Inter (Any Implicit, Void, [NumLit "1"]), Void, []), []) in
    let t_out = S2.run t_in in
    let t_exp = Union (Void, Inter (Any Implicit, Void,[NumLit "1"]), []) in
    assert_equal ~ctxt ~printer:Ty.show t_exp t_out;
  end;

]
