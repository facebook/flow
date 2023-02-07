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

let jsdoc_param_of_ty_param (name, _, _) = (Base.Option.value ~default:"" name, [])

let stub_for_ty =
  let open Ty in
  function
  | Fun { fun_params; _ } ->
    Some { description = "$0"; params = Base.List.map ~f:jsdoc_param_of_ty_param fun_params }
  | _ -> None

let string_of_stub { description; params } =
  let params =
    params
    |> Base.List.mapi ~f:(fun i (name, _) -> Printf.sprintf "\n * @param %s $%d" name (i + 1))
    |> Base.String.concat
  in
  Printf.sprintf "/**\n * %s%s\n */" description params
