(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Codemod_empty_annotator = Codemod_annotator.Make (Insert_type_utils.UnitStats)
module Acc = Insert_type_utils.Acc (Insert_type_utils.UnitStats)

let mapper
    ~preserve_literals ~generalize_maybe ~max_type_size ~default_any (cctx : Codemod_context.Typed.t)
    =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  object (this)
    inherit
      Codemod_empty_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe
        ~lint_severities
        ~max_type_size
        ~preserve_literals
        ~merge_arrays:true
        () as super

    method private post_run () = ()

    method private get_annot ploc ty annot =
      let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
      let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
      this#opt_annotate ~f ~error ~expr:None ploc ty annot

    method! variable_declarator ~kind decl =
      let open Ast.Expression in
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (loc, { id; init }) = decl in
      (* We are matching:
       * `const [x, y] = useState(<>);`
       * (or `React.useState` instead of `useState`)
       * and producing:
       * `const [x, y] = useState<T>(<>);`
       * where `T` is the inferred type of `x`
       *)
      match (id, init) with
      | ( ( _,
            Ast.Pattern.Array
              (* Array literal with two elements, the first being an identifier. *)
              {
                Ast.Pattern.Array.elements =
                  [
                    Ast.Pattern.Array.Element
                      ( _,
                        {
                          Ast.Pattern.Array.Element.argument = (val_loc, Ast.Pattern.Identifier _);
                          _;
                        }
                      );
                    Ast.Pattern.Array.Element _;
                  ];
                annot = Ast.Type.Missing _;
                _;
              }
          ),
          Some
            ( call_loc,
              Call
                (* Call to `useState` or `React.useState` *)
                {
                  Call.callee =
                    ( (_, Identifier (_, { Ast.Identifier.name = "useState"; _ }))
                    | ( _,
                        Member
                          {
                            Member._object =
                              (_, Identifier (_, { Ast.Identifier.name = "React"; _ }));
                            property =
                              Member.PropertyIdentifier (_, { Ast.Identifier.name = "useState"; _ });
                            _;
                          }
                      ) ) as callee;
                  targs = None;
                  arguments =
                    ( _,
                      {
                        ArgList.arguments =
                          [
                            Expression
                              (* `null` *)
                              ( (_, Literal { Ast.Literal.value = Ast.Literal.Null; _ })
                              (* `undefined` *)
                              | (_, Identifier (_, { Ast.Identifier.name = "undefined"; _ }))
                              (* `{}` *)
                              | (_, Object { Object.properties = []; _ })
                              (* `[]` *)
                              | (_, Array { Array.elements = []; _ }) );
                          ];
                        _;
                      }
                    ) as arguments;
                  comments;
                }
            )
        ) ->
        let ty_result =
          Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size val_loc
        in
        (match ty_result with
        (* Don't insert the empty inexact object type `{...}`, `Array<any>`, just `void`, or just `null`.
           They aren't useful for our purposes. *)
        | Ok
            ( Ty.Obj { Ty.obj_kind = Ty.InexactObj; obj_props = []; _ }
            | Ty.Arr { Ty.arr_elt_t = Ty.Any _ | Ty.Bot _; _ }
            | Ty.Null | Ty.Void ) ->
          super#variable_declarator ~kind decl
        | Ok _ ->
          (match this#get_annot loc ty_result (Ast.Type.Missing call_loc) with
          | Ast.Type.Available (_, t) ->
            (* Add the explicit type argument to `useState`. *)
            let targs =
              Some (call_loc, { CallTypeArgs.arguments = [CallTypeArg.Explicit t]; comments = None })
            in
            ( loc,
              {
                Ast.Statement.VariableDeclaration.Declarator.id;
                init = Some (call_loc, Call { Call.callee; targs; arguments; comments });
              }
            )
          | _ -> super#variable_declarator ~kind decl)
        | _ -> super#variable_declarator ~kind decl)
      | _ -> super#variable_declarator ~kind decl
  end
