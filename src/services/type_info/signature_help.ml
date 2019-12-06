(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let parameter_name is_opt name =
  let opt =
    if is_opt then
      "?"
    else
      ""
  in
  Option.value name ~default:"_" ^ opt

let func_details params rest_param return =
  let param_tys =
    Base.List.map
      ~f:(fun (n, t, fp) ->
        let param_name = parameter_name fp.Ty.prm_optional n in
        let param_ty = Ty_printer.string_of_t ~with_comments:false t in
        { ServerProt.Response.param_name; param_ty })
      params
  in
  let param_tys =
    match rest_param with
    | None -> param_tys
    | Some (name, t) ->
      let param_name = "..." ^ parameter_name false name in
      let param_ty = Ty_printer.string_of_t ~with_comments:false t in
      param_tys @ [{ ServerProt.Response.param_name; param_ty }]
  in
  let return_ty = Ty_printer.string_of_t ~with_comments:false return in
  { ServerProt.Response.param_tys; return_ty }
