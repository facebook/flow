(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target_loc kind =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method! type_ t =
      let open Flow_ast.Type in
      match t with
      | ( loc,
          Generic
            {
              Generic.id =
                Generic.Identifier.Unqualified
                  (_, { Flow_ast.Identifier.name = "$Keys"; comments = _ });
              targs = Some (_, { TypeArgs.arguments = [targ]; comments = _ });
              comments;
            }
        )
        when this#is_target loc && kind = `DollarKeys ->
        (loc, Keyof { Keyof.argument = targ; comments })
      | _ -> super#type_ t
  end

let convert_dollar_keys_type ast loc =
  let mapper = new mapper loc `DollarKeys in
  mapper#program ast
