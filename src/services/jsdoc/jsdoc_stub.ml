(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  description: string;
  params: Jsdoc.Params.t;
}

let stub_for_function
    { Flow_ast.Function.params = (_, { Flow_ast.Function.Params.this_; params; rest; _ }); _ } =
  let params =
    let jsdoc_param_of_pattern (_, pattern) =
      let open Flow_ast.Pattern in
      match pattern with
      | Identifier { Identifier.name = (_, { Flow_ast.Identifier.name; _ }); _ } -> (name, [])
      (* TODO: handle other kinds of patterns *)
      | Object _
      | Array _
      | Expression _ ->
        ("", [])
    in
    let add_this_param this_ jsdoc_params =
      match this_ with
      | Some _ -> ("this", []) :: jsdoc_params
      | None -> jsdoc_params
    in
    let add_params params jsdoc_params =
      Base.List.rev_map_append
        params
        jsdoc_params
        ~f:(fun (_, { Flow_ast.Function.Param.argument; _ }) -> jsdoc_param_of_pattern argument
      )
    in
    let add_rest_param rest jsdoc_params =
      match rest with
      | Some (_, { Flow_ast.Function.RestParam.argument; _ }) ->
        jsdoc_param_of_pattern argument :: jsdoc_params
      | None -> jsdoc_params
    in
    let params_rev = [] |> add_this_param this_ |> add_params params |> add_rest_param rest in
    Base.List.rev params_rev
  in
  { description = "$0"; params }

let string_of_stub { description; params } =
  let params =
    params
    |> Base.List.mapi ~f:(fun i (name, _) -> Printf.sprintf "\n * @param %s $%d" name (i + 1))
    |> Base.String.concat
  in
  Printf.sprintf "*\n * %s%s\n" description params
