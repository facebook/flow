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
    ~preserve_literals
    ~generalize_maybe
    ~generalize_react_mixed_element
    ~max_type_size
    ~default_any
    (cctx : Codemod_context.Typed.t) =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  object (this)
    inherit
      Codemod_empty_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe
        ~generalize_react_mixed_element
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
                              | (_, Array { Array.elements = []; _ })
                              (* `new Set()` | `new Map()` *)
                              | ( _,
                                  New
                                    {
                                      New.callee =
                                        ( _,
                                          Identifier (_, { Ast.Identifier.name = "Map" | "Set"; _ })
                                        );
                                      targs = None;
                                      arguments = None | Some (_, { ArgList.arguments = []; _ });
                                      _;
                                    }
                                ) );
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
        (* Ignore things like `empty`, `Array<empty>`, `Set<empty>`, `Map<empty, empty>`, `{...}` *)
        let ignore_type = function
          | Ty.Any _
          | Ty.Bot _
          | Ty.Arr { Ty.arr_elt_t = Ty.Any _ | Ty.Bot _; _ }
          | Ty.Generic ({ Ty_symbol.sym_name = Reason.OrdinaryName "Set"; _ }, _, Some [Ty.Bot _])
          | Ty.Generic
              ({ Ty_symbol.sym_name = Reason.OrdinaryName "Map"; _ }, _, Some [Ty.Bot _; Ty.Bot _])
          | Ty.Obj { Ty.obj_kind = Ty.InexactObj; obj_props = []; _ } ->
            true
          | _ -> false
        in
        (* Filter out undesirable types from unions. *)
        let ty_result =
          match ty_result with
          | Ok (Ty.Union _ as ty) ->
            let members = Ty.bk_union ty |> Nel.to_list in
            let filtered = Base.List.filter ~f:(fun ty -> not @@ ignore_type ty) members in
            (match filtered with
            | [] -> Ok (Ty.Bot Ty.EmptyType)
            | [x] -> Ok x
            | x0 :: x1 :: xs -> Ok (Ty.Union (false, x0, x1, xs)))
          | _ -> ty_result
        in
        (match ty_result with
        (* Don't insert just `void`or just `null` (but these are OK in a union). *)
        | Ok (Ty.Null | Ty.Void) -> super#variable_declarator ~kind decl
        | Ok t when ignore_type t -> super#variable_declarator ~kind decl
        | Ok _ ->
          (match this#get_annot loc ty_result (Ast.Type.Missing call_loc) with
          | Ast.Type.Available (_, t) ->
            (match (arguments, t) with
            | ( ( args_loc,
                  {
                    ArgList.arguments =
                      [
                        (* Match expression `new Set()` *)
                        Expression
                          ( new_loc,
                            New
                              {
                                New.callee =
                                  (_, Identifier (_, { Ast.Identifier.name = "Set"; _ })) as
                                  new_callee;
                                targs = None;
                                arguments =
                                  (None | Some (_, { ArgList.arguments = []; _ })) as new_arguments;
                                comments = new_comments;
                              }
                          );
                      ];
                    comments = args_comments;
                  }
                ),
                ( _,
                  Ast.Type.Generic
                    {
                      (* Match type `Set<T>` *)
                      Ast.Type.Generic.id =
                        Ast.Type.Generic.Identifier.Unqualified
                          (_, { Ast.Identifier.name = "Set"; _ });
                      targs =
                        Some
                          ( targs_loc,
                            { Ast.Type.TypeArgs.arguments = [targ_t]; comments = targs_comments }
                          );
                      _;
                    }
                )
              ) ->
              let new_targs =
                ( targs_loc,
                  {
                    CallTypeArgs.arguments = [CallTypeArg.Explicit targ_t];
                    comments = targs_comments;
                  }
                )
              in
              let arguments =
                ( args_loc,
                  {
                    ArgList.arguments =
                      [
                        Expression
                          ( new_loc,
                            New
                              {
                                New.callee = new_callee;
                                targs = Some new_targs;
                                arguments = new_arguments;
                                comments = new_comments;
                              }
                          );
                      ];
                    comments = args_comments;
                  }
                )
              in
              (* Add explicit type argument to the `new Set()`. *)
              ( loc,
                {
                  Ast.Statement.VariableDeclaration.Declarator.id;
                  init = Some (call_loc, Call { Call.callee; targs = None; arguments; comments });
                }
              )
            | ( ( args_loc,
                  {
                    ArgList.arguments =
                      [
                        (* Match expression `new Map()` *)
                        Expression
                          ( new_loc,
                            New
                              {
                                New.callee =
                                  (_, Identifier (_, { Ast.Identifier.name = "Map"; _ })) as
                                  new_callee;
                                targs = None;
                                arguments =
                                  (None | Some (_, { ArgList.arguments = []; _ })) as new_arguments;
                                comments = new_comments;
                              }
                          );
                      ];
                    comments = args_comments;
                  }
                ),
                ( _,
                  Ast.Type.Generic
                    {
                      (* Match type `Map<K, V>` *)
                      Ast.Type.Generic.id =
                        Ast.Type.Generic.Identifier.Unqualified
                          (_, { Ast.Identifier.name = "Map"; _ });
                      targs =
                        Some
                          ( targs_loc,
                            {
                              Ast.Type.TypeArgs.arguments = [targ_t0; targ_t1];
                              comments = targs_comments;
                            }
                          );
                      _;
                    }
                )
              ) ->
              let new_targs =
                ( targs_loc,
                  {
                    CallTypeArgs.arguments =
                      [CallTypeArg.Explicit targ_t0; CallTypeArg.Explicit targ_t1];
                    comments = targs_comments;
                  }
                )
              in
              let arguments =
                ( args_loc,
                  {
                    ArgList.arguments =
                      [
                        Expression
                          ( new_loc,
                            New
                              {
                                New.callee = new_callee;
                                targs = Some new_targs;
                                arguments = new_arguments;
                                comments = new_comments;
                              }
                          );
                      ];
                    comments = args_comments;
                  }
                )
              in
              (* Add explicit type argument to the `new Map()`. *)
              ( loc,
                {
                  Ast.Statement.VariableDeclaration.Declarator.id;
                  init = Some (call_loc, Call { Call.callee; targs = None; arguments; comments });
                }
              )
            | _ ->
              (* Add the explicit type argument to `useState`. *)
              let targs =
                Some
                  (call_loc, { CallTypeArgs.arguments = [CallTypeArg.Explicit t]; comments = None })
              in
              ( loc,
                {
                  Ast.Statement.VariableDeclaration.Declarator.id;
                  init = Some (call_loc, Call { Call.callee; targs; arguments; comments });
                }
              ))
          | _ -> super#variable_declarator ~kind decl)
        | _ -> super#variable_declarator ~kind decl)
      | _ -> super#variable_declarator ~kind decl

    method private use_callback_get_annot ploc annot =
      match annot with
      | Ast.Type.Available _ -> annot
      | Ast.Type.Missing _ ->
        let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
        let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
        let ty_result =
          Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size ploc
          |> Base.Result.bind ~f:(function
                 | Ty.Bot _ ->
                   Error [Insert_type_utils.Error.Missing_annotation_or_normalizer_error]
                 | t -> Ok t
                 )
        in
        this#opt_annotate ~f ~error ~expr:None ploc ty_result annot

    method private annotate_use_callback_fn_param ploc patt =
      match patt with
      | Ast.Pattern.Object Ast.Pattern.Object.{ annot; properties; comments } ->
        let annot' = this#use_callback_get_annot ploc annot in
        (ploc, Ast.Pattern.Object Ast.Pattern.Object.{ annot = annot'; properties; comments })
      | Ast.Pattern.Array Ast.Pattern.Array.{ annot; elements; comments } ->
        let annot' = this#use_callback_get_annot ploc annot in
        (ploc, Ast.Pattern.Array Ast.Pattern.Array.{ annot = annot'; elements; comments })
      | Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot; name; optional } ->
        let annot' = this#use_callback_get_annot ploc annot in
        (ploc, Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot = annot'; name; optional })
      | Ast.Pattern.Expression _ ->
        (* No such thing as a pattern expression *)
        (ploc, patt)

    method private annotate_use_callback_fn fn =
      let {
        Ast.Function.params = (params_loc, { Ast.Function.Params.this_; params; rest; comments });
        _;
      } =
        fn
      in
      let params =
        Base.List.map
          params
          ~f:(fun (loc, { Ast.Function.Param.argument = (ploc, patt); default }) ->
            ( loc,
              {
                Ast.Function.Param.argument = this#annotate_use_callback_fn_param ploc patt;
                default;
              }
            )
        )
      in
      {
        fn with
        Ast.Function.params = (params_loc, { Ast.Function.Params.this_; params; rest; comments });
      }

    method! call loc expr =
      let open Ast.Expression in
      match expr with
      | {
       (* Call to `useCallback` or `React.useCallback` *)
       Call.callee =
         ( (_, Identifier (_, { Ast.Identifier.name = "useCallback"; _ }))
         | ( _,
             Member
               {
                 Member._object = (_, Identifier (_, { Ast.Identifier.name = "React"; _ }));
                 property = Member.PropertyIdentifier (_, { Ast.Identifier.name = "useCallback"; _ });
                 _;
               }
           ) ) as callee;
       targs = None;
       arguments = (arg_loc, { ArgList.arguments = Expression e :: rest; comments = arg_comments });
       comments;
      } ->
        (* useCallback((a, b) => {...}, ...) or useCallback(function(a, b) {...}, ...) *)
        (match e with
        | (loc, ArrowFunction fn) ->
          let fn_arg = Expression (loc, ArrowFunction (this#annotate_use_callback_fn fn)) in
          {
            Call.callee;
            targs = None;
            arguments = (arg_loc, { ArgList.arguments = fn_arg :: rest; comments = arg_comments });
            comments;
          }
        | (loc, Function fn) ->
          let fn_arg = Expression (loc, Function (this#annotate_use_callback_fn fn)) in
          {
            Call.callee;
            targs = None;
            arguments = (arg_loc, { ArgList.arguments = fn_arg :: rest; comments = arg_comments });
            comments;
          }
        | _ -> super#call loc expr)
      | _ -> super#call loc expr
  end
