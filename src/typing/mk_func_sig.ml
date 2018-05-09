module Flow = Flow_js
module Anno = Type_annotation

open Reason
open Func_sig

let function_kind {Ast.Function.async; generator; predicate; _ } =
  Ast.Type.Predicate.(match async, generator, predicate with
  | true, true, None -> AsyncGenerator
  | true, false, None -> Async
  | false, true, None -> Generator
  | false, false, None -> Ordinary
  | false, false, Some (_, Declared _) -> Predicate
  | false, false, Some (_ , Inferred) -> Predicate
  | _, _, _ -> Utils_js.assert_false "(async || generator) && pred")

let mk_params cx tparams_map ~expr func =
  let add_param_with_default default = function
    | loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = (_, name) as id;
        annot;
        optional;
      } ->
      let reason = mk_reason (RParameter (Some name)) loc in
      let t = Anno.mk_type_annotation cx tparams_map reason annot in
      Func_params.add_simple cx ~tparams_map ~optional ?default loc (Some id) t
    | loc, _ as patt ->
      let reason = mk_reason RDestructuring loc in
      let annot = Destructuring.type_of_pattern patt in
      let t = Anno.mk_type_annotation cx tparams_map reason annot in
      Func_params.add_complex cx ~tparams_map ~expr ?default patt t
  in
  let add_rest patt params =
    match patt with
    | loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = (_, name) as id;
        annot;
        _;
      } ->
      let reason = mk_reason (RRestParameter (Some name)) loc in
      let t = Anno.mk_type_annotation cx tparams_map reason annot in
      Func_params.add_rest cx ~tparams_map loc (Some id) t params
    | loc, _ ->
      Flow_js.add_output cx
        Flow_error.(EInternal (loc, RestParameterNotIdentifierPattern));
      params
  in
  let add_param = function
    | _, Ast.Pattern.Assignment { Ast.Pattern.Assignment.left; right; } ->
      add_param_with_default (Some right) left
    | patt ->
      add_param_with_default None patt
  in
  let {Ast.Function.params = (_, { Ast.Function.Params.params; rest }); _} = func in
  let params = List.fold_left (fun acc param ->
    add_param param acc
  ) Func_params.empty params in
  match rest with
  | Some (_, { Ast.Function.RestElement.argument }) -> add_rest argument params
  | None -> params

let mk cx tparams_map ~expr loc func =
  let {Ast.Function.tparams; return; body; predicate; _} = func in
  let reason = func_reason func loc in
  let kind = function_kind func in
  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx ~tparams_map tparams
  in
  let fparams = mk_params cx tparams_map ~expr func in
  let body = Some body in
  let ret_reason = mk_reason RReturn (return_loc func) in
  let return_t =
    Anno.mk_type_annotation cx tparams_map ret_reason return
  in
  let return_t = Ast.Type.Predicate.(match predicate with
    | None ->
        return_t
    | Some (_, Inferred) ->
        (* Restrict the fresh condition type by the declared return type *)
        let fresh_t = Anno.mk_type_annotation cx tparams_map ret_reason None in
        Flow.flow_t cx (fresh_t, return_t);
        fresh_t
    | Some (loc, Declared _) ->
        Flow_js.add_output cx Flow_error.(
          EUnsupportedSyntax (loc, PredicateDeclarationForImplementation)
        );
        Anno.mk_type_annotation cx tparams_map ret_reason None
  ) in
  {reason; kind; tparams; tparams_map; fparams; body; return_t}
