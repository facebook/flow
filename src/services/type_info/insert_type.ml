(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
class mapper ty_query target = object(this)
  inherit Suggest.visitor ty_query as super

  method private contains_target loc = Reason.in_range target loc

  method! function_param_pattern (((loc, _) as patt): (Loc.t, Loc.t) Flow_ast.Pattern.t) =
    if this#contains_target loc
    then begin
      super#function_param_pattern patt end
    else begin
      patt
    end

  method! callable_return blame_loc func =
    let open Flow_ast.Function in
    match func with
    | {return=(Flow_ast.Type.Missing loc); _} when this#contains_target loc ->
      super#callable_return blame_loc func
    | _ -> func
end
