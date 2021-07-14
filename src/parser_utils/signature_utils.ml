(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_visitor

module Procedure_decider = struct
  class decider =
    object (this)
      inherit [bool, Loc.t] visitor ~init:true

      method private no = this#update_acc (fun _ -> false)

      method! function_ _loc (expr : (Loc.t, Loc.t) Flow_ast.Function.t) = expr

      method! return _loc (stmt : (Loc.t, Loc.t) Flow_ast.Statement.Return.t) =
        let open Flow_ast.Statement.Return in
        let { argument; comments = _ } = stmt in
        begin
          match argument with
          | None -> ()
          | Some _ -> this#no
        end;
        stmt

      method! function_body_any (body : (Loc.t, Loc.t) Flow_ast.Function.body) =
        begin
          match body with
          | Flow_ast.Function.BodyBlock block -> ignore @@ this#function_body block
          | Flow_ast.Function.BodyExpression _ -> this#no
        end;
        body
    end

  let is (body : (Loc.t, Loc.t) Flow_ast.Function.body) =
    let decider = new decider in
    decider#eval decider#function_body_any body
end

let is_munged_property_string name = String.length name >= 2 && name.[0] = '_' && name.[1] <> '_'

let is_munged_property_name = function
  (* TODO consider adding another name variant for munged property strings *)
  | Reason.OrdinaryName name -> is_munged_property_string name
  | Reason.InternalName _
  | Reason.InternalModuleName _ ->
    false

module This_finder = struct
  class ['a] finder =
    object (this)
      inherit [bool, 'a] visitor ~init:false

      method! this_expression _ node =
        this#set_acc true;
        node

      (* Any mentions of `this` in these constructs would reference
         the `this` within those structures, so we ignore them *)
      method! class_ _ x = x

      method! function_declaration _ x = x

      method! function_expression _ x = x
    end

  let found_this_in_body_or_params
      (body : ('a, 'a) Flow_ast.Function.body) (params : ('a, 'a) Flow_ast.Function.Params.t) =
    let finder = new finder in
    (* If this appears in parameter defaults it still counts *)
    finder#eval finder#function_body_any body || finder#eval finder#function_params params
end
