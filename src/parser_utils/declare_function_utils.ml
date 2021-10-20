(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Transform predicate declare functions to functions whose body is the
   predicate declared for the funcion *)
(* Also returns a function for reversing this process, for the sake of
   typed AST construction. *)
(* TODO: Would be better to handle declared functions directly rather than
   doing this dance *)
type 'l declare_function_error =
  | PredicateDeclarationWithoutExpression of 'l
  | PredicateDeclarationAnonymousParameters of 'l

let declare_function_to_function_declaration
    (type l lt)
    ~loc_of_tloc
    ~copy_t
    ~add_output
    declare_loc
    (func_decl : (l, l) Flow_ast.Statement.DeclareFunction.t) :
    ( (l, l) Flow_ast.Statement.t'
    * ((l, lt) Flow_ast.Statement.t -> (l, lt) Flow_ast.Statement.DeclareFunction.t)
    )
    option =
  let { Flow_ast.Statement.DeclareFunction.id; annot; predicate; comments } = func_decl in
  match predicate with
  | Some (loc, { Flow_ast.Type.Predicate.kind = Flow_ast.Type.Predicate.Inferred; comments = _ }) ->
    add_output (PredicateDeclarationWithoutExpression loc);
    None
  | Some
      ( (loc : l),
        {
          Flow_ast.Type.Predicate.kind = Flow_ast.Type.Predicate.Declared e;
          comments = pred_comments;
        }
      ) ->
    begin
      match annot with
      | ( annot_loc,
          ( func_annot_loc,
            Flow_ast.Type.Function
              {
                Flow_ast.Type.Function.params =
                  ( params_loc,
                    {
                      Flow_ast.Type.Function.Params.params;
                      rest;
                      this_;
                      comments = params_comments;
                    }
                  );
                Flow_ast.Type.Function.return;
                Flow_ast.Type.Function.tparams;
                comments = func_comments;
              }
          )
        ) ->
        let param_type_to_param : (l, l) Flow_ast.Type.Function.Param.t -> (l, l) Flow_ast.Pattern.t
            =
          let open Flow_ast.Type.Function in
          fun (l, { Param.name; Param.annot; _ }) ->
            let name =
              match name with
              | Some name -> name
              | None ->
                let name_loc = fst annot in
                add_output (PredicateDeclarationAnonymousParameters loc);
                (name_loc, { Flow_ast.Identifier.name = "_"; comments })
            in
            let name' =
              {
                Flow_ast.Pattern.Identifier.name;
                annot = Flow_ast.Type.Available (fst annot, annot);
                optional = false;
              }
            in
            (l, Flow_ast.Pattern.Identifier name')
        in
        let params : (l, l) Flow_ast.Function.Param.t list =
          Base.List.map
            ~f:(fun param ->
              let ((loc, _) as argument) = param_type_to_param param in
              (loc, { Flow_ast.Function.Param.argument; default = None }))
            params
        in
        let rest : (l, l) Flow_ast.Function.RestParam.t option =
          let open Flow_ast.Type.Function in
          match rest with
          | Some (rest_loc, { RestParam.argument; comments }) ->
            let argument = param_type_to_param argument in
            Some (rest_loc, { Flow_ast.Function.RestParam.argument; comments })
          | None -> None
        in
        let this_ : (l, l) Flow_ast.Function.ThisParam.t option =
          let open Flow_ast.Type.Function in
          match this_ with
          | Some (this_loc, { ThisParam.annot; comments }) ->
            Some (this_loc, { Flow_ast.Function.ThisParam.annot; comments })
          | None -> None
        in
        let body : (l, l) Flow_ast.Function.body =
          Flow_ast.Function.BodyBlock
            ( loc,
              {
                Flow_ast.Statement.Block.body =
                  [
                    ( loc,
                      Flow_ast.Statement.Return
                        { Flow_ast.Statement.Return.argument = Some e; comments = None }
                    );
                  ];
                comments = None;
              }
            )
        in
        let return : (l, l) Flow_ast.Type.annotation_or_hint =
          Flow_ast.Type.Available (loc, return)
        in
        Some
          ( Flow_ast.Statement.FunctionDeclaration
              {
                Flow_ast.Function.id = Some id;
                params =
                  (params_loc, { Flow_ast.Function.Params.params; rest; this_; comments = None });
                body;
                async = false;
                generator = false;
                predicate =
                  Some
                    ( loc,
                      {
                        Flow_ast.Type.Predicate.kind = Flow_ast.Type.Predicate.Inferred;
                        comments = None;
                      }
                    );
                return;
                tparams;
                sig_loc = declare_loc;
                comments = None;
              },
            function
            | ( _,
                Flow_ast.Statement.FunctionDeclaration
                  {
                    Flow_ast.Function.id = Some (id_loc_fun_type, id_name);
                    tparams;
                    params =
                      (params_loc, { Flow_ast.Function.Params.params; rest; this_; comments = _ });
                    return = Flow_ast.Type.Available (_, return);
                    body =
                      Flow_ast.Function.BodyBlock
                        ( pred_loc,
                          {
                            Flow_ast.Statement.Block.body =
                              [
                                ( _,
                                  Flow_ast.Statement.Return
                                    { Flow_ast.Statement.Return.argument = Some e; comments = _ }
                                );
                              ];
                            comments = _;
                          }
                        );
                    _;
                  }
              ) ->
              let param_to_param_type :
                  (l, lt) Flow_ast.Pattern.t -> (l, lt) Flow_ast.Type.Function.Param.t = function
                | ( loc_t,
                    Flow_ast.Pattern.Identifier
                      {
                        Flow_ast.Pattern.Identifier.name = (name_loc_t, name);
                        annot = Flow_ast.Type.Available (_, annot);
                        optional;
                      }
                  ) ->
                  ( loc_of_tloc loc_t,
                    {
                      Flow_ast.Type.Function.Param.name =
                        Some (copy_t loc_t (loc_of_tloc name_loc_t), name);
                      annot;
                      optional;
                    }
                  )
                | _ -> Utils_js.assert_false "Function declaration AST has unexpected shape"
              in
              let params =
                Base.List.map
                  ~f:(fun (_, { Flow_ast.Function.Param.argument; default }) ->
                    if default <> None then
                      Utils_js.assert_false "Function declaration AST has unexpected shape";
                    param_to_param_type argument)
                  params
              in
              let rest =
                Base.Option.map
                  ~f:(fun (rest_loc, { Flow_ast.Function.RestParam.argument; comments }) ->
                    ( rest_loc,
                      {
                        Flow_ast.Type.Function.RestParam.argument = param_to_param_type argument;
                        comments;
                      }
                    ))
                  rest
              in
              let this_ =
                Base.Option.map
                  ~f:(fun (this_loc, { Flow_ast.Function.ThisParam.annot; comments }) ->
                    (this_loc, { Flow_ast.Type.Function.ThisParam.annot; comments }))
                  this_
              in
              let annot =
                ( annot_loc,
                  ( copy_t id_loc_fun_type func_annot_loc,
                    Flow_ast.Type.Function
                      {
                        Flow_ast.Type.Function.params =
                          ( params_loc,
                            {
                              Flow_ast.Type.Function.Params.params;
                              rest;
                              this_;
                              comments = params_comments;
                            }
                          );
                        return;
                        tparams;
                        comments = func_comments;
                      }
                  )
                )
              in
              {
                Flow_ast.Statement.DeclareFunction.id = (id_loc_fun_type, id_name);
                annot;
                predicate =
                  Some
                    ( pred_loc,
                      {
                        Flow_ast.Type.Predicate.kind = Flow_ast.Type.Predicate.Declared e;
                        comments = pred_comments;
                      }
                    );
                comments;
              }
            | _ -> failwith "Internal error: malformed predicate declare function"
          )
      | _ -> None
    end
  | _ -> None

let declare_function_to_function_declaration_simple loc st =
  declare_function_to_function_declaration
    ~loc_of_tloc:(fun x -> x)
    ~copy_t:(fun _ x -> x)
    ~add_output:(Fn.const ())
    loc
    st
  |> Base.Option.map ~f:fst
