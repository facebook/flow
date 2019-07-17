(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

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

  (* When simplify_empty is true, all kinds of `empty` are equivalent, even when
   * nested under a type constructor.
   *
   * {f: empty} | {f: empty'}
   * ~> (simplify_empty:true)
   * {f: empty} | {f: empty'}
   *
   * TODO: This should simplify to {f: empty}
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
    let t_exp = Ty.Union (
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
]
