(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let spf = Printf.sprintf

let exports_map cx module_name =
  let module_map = Context.module_map cx in
  match SMap.get module_name module_map with
  | Some module_t -> (
    let module_t = Flow_js.resolve_type cx module_t in
    match Flow_js.Autocomplete.extract_members cx module_t with
    | Flow_js.Autocomplete.SuccessModule (named, cjs) -> (named, cjs)
    | _ -> failwith (
      spf "Failed to extract the exports of %s" (Type.string_of_ctor module_t)
    )
  )
  | None ->
    failwith (spf "Unable to extract %s from the module_map!" module_name)

let gen_class_body =
  let rec gen_method method_name t env = Codegen.(Type.(
    match resolve_type t env with
    | AnnotT (_, t) -> gen_method method_name (resolve_type t env) env
    | FunT (_, _static, _super, {params_tlist; params_names; return_t; _;}) ->
      let is_empty_constructor =
        method_name = "constructor"
        && List.length params_tlist = 0
        && match resolve_type return_t env with VoidT _ -> true | _ -> false
      in
      if is_empty_constructor then env else (
        add_str "  " env
          |> add_str method_name
          |> gen_tparams_list
          |> add_str "("
          |> gen_fun_params params_names params_tlist
          |> add_str "): "
          |> gen_type return_t
          |> add_str ";\n"
      )
    | PolyT (tparams, t) ->
      add_tparams tparams env |> gen_method method_name (resolve_type t env)
    | t ->
      add_str "  " env
        |> add_str method_name
        |> add_str "() {} //"
        |> add_str (Type.string_of_ctor t)
        |> add_str "\n"
  )) in

  fun fields methods env -> Codegen.(
    let fields_count = SMap.cardinal fields in
    let methods_count = SMap.cardinal methods in

    let env = add_str " {" env in
    if fields_count + methods_count = 0 then add_str "}" env else (
      add_str "\n" env
        |> SMap.fold (fun field_name t env ->
           add_str "  " env
             |> add_str field_name
             |> add_str ": "
             |> gen_type t
             |> add_str ";\n"
        ) fields
        (* There's always a "constructor" method *)
        |> (if methods_count > 1 then add_str "\n" else (fun env -> env))
        |> SMap.fold gen_method methods
        |> add_str "}"
  )
)

let gen_named_exports =
  let rec fold_named_export name t env = Codegen.(Type.(
    let env = (
      match resolve_type t env with
      | FunT (_, _static, _prototype, {
          params_tlist;
          params_names;
          return_t;
          _;
        }) ->
        add_str "declare export function " env
          |> add_str name
          |> gen_tparams_list
          |> add_str "("
          |> gen_fun_params params_names params_tlist
          |> add_str "): "
          |> gen_type return_t
          |> add_str ";"

      | PolyT (tparams, t) -> add_tparams tparams env |> fold_named_export name t

      | ThisClassT (InstanceT (_, _static, super, {
          fields_tmap;
          methods_tmap;
          (* TODO: The only way to express `mixins` right now is with a
           *       `declare` statement. This is possible in implementation
           *       files, but it is extremely rare -- so punting on this for
           *       now.
           *)
          mixins = _;
          _;
        })) ->
        let fields = Codegen.find_props fields_tmap env in
        let methods = Codegen.find_props methods_tmap env in
        let env = add_str "declare export class " env
          |> add_str name
          |> gen_tparams_list
        in
        let env = (
          match Codegen.resolve_type super env with
          | MixedT _ -> env
          | (ThisTypeAppT _) as t -> add_str " extends " env |> gen_type t
          | _ -> failwith (
            spf "Unexpected super type for class: %s" (string_of_ctor super)
          )
        ) in
        gen_class_body fields methods env

      | TypeT (_, t) ->
        add_str "export type " env
          |> add_str name
          |> gen_tparams_list
          |> add_str " = "
          |> gen_type t
          |> add_str ";"

      | t ->
        add_str "declare export var " env
          |> add_str name
          |> add_str ": "
          |> gen_type t
          |> add_str ";"
    ) in
    add_str "\n" env
  )) in
  SMap.fold fold_named_export

class unexported_class_visitor = object(self)
  inherit [Codegen.codegen_env * Type.TypeSet.t] Type_visitor.t as super

  method! tvar cx (env, seen) r id =
    let t = Codegen.resolve_type (Type.OpenT (r, id)) env in
    self#type_ cx (env, seen) t

  method! type_ cx (env, seen) t = Codegen.(Type.(
    if TypeSet.mem t seen then (env, seen) else (
      let seen = TypeSet.add t seen in
      match t with
      (* class_id = 0 is top of the inheritance chain *)
      | InstanceT (_, _, _, {class_id; _;}) when class_id = 0 -> (env, seen)

      | InstanceT (r, _static, extends, {
          class_id;
          fields_tmap;
          methods_tmap;
          _;
        }) when not (has_declared_class class_id env || Reason.is_lib_reason r) ->
        let class_name_id = next_declared_class_name env in
        let class_name = spf "Class%d" class_name_id in

        (**
         * Add to the list of declared classes *FIRST* to prevent inifite loops
         * on recursive references to this class from within itself.
         *)
        let env = add_declared_class class_id class_name env in
        let (env, seen) = super#type_ cx (env, seen) t in

        let env = add_str "declare class " env |> add_str class_name in

        let env =
          match resolve_type extends env with
          | MixedT _ -> env
          | ClassT t when (
              match resolve_type t env with | MixedT _ -> true | _ -> false
            ) -> env
          | ThisTypeAppT (extends, _, ts) ->
            add_str " extends " env
              |> gen_type extends
              |> add_str "<"
              |> gen_separated_list ts ", " gen_type
              |> add_str ">"
          | extends -> add_str " extends " env |> gen_type extends
        in

        let fields = find_props fields_tmap env in
        let methods = find_props methods_tmap env in
        let env = gen_class_body fields methods env |> add_str "\n" in
        (env, seen)

      | t -> super#type_ cx (env, seen) t
    )
  ))
end

let declare_classes =
  let rec mark_declared_classes name t env = Codegen.(Type.(
    match t with
    | ThisClassT (InstanceT (_, _, _, {class_id; _;})) ->
      add_declared_class class_id name env
    | PolyT (_, t) ->
      mark_declared_classes name t env
    | _ ->
      env
  )) in

  let visitor = new unexported_class_visitor in
  let gen_unexported_classes _name t env =
    fst (visitor#type_ env.Codegen.flow_cx (env, Type.TypeSet.empty) t)
  in

  fun named_exports cjs_export env ->
    (* Find and mark all the declared *exported* classes first *)
    let env = SMap.fold mark_declared_classes named_exports env in

    (* Codegen any referenced, non-exported classes *)
    let all_exports =
      match cjs_export with
      | None -> named_exports
      | Some cjs_t -> SMap.add "*CJS*" cjs_t named_exports
    in
    SMap.fold gen_unexported_classes all_exports env

let generate_interface cx =
  let module_name = Modulename.to_string (Context.module_name cx) in
  let (named_exports, cjs_export) = exports_map cx module_name in

  let env = Codegen.mk_env cx (Buffer.create 320) in
  let env = declare_classes named_exports cjs_export env in
  Codegen.to_string (
    match cjs_export with
    | None -> gen_named_exports named_exports env
    | Some cjs_t ->
      let type_exports = SMap.filter Type.(fun _name t ->
        let t = match t with OpenT _ -> Codegen.resolve_type t env | _ -> t in
        match t with
        | TypeT _ | PolyT (_, TypeT _) -> true
        | _ -> false
      ) named_exports in
      gen_named_exports type_exports env
        |> Codegen.add_str "\ndeclare module.exports: "
        |> Codegen.gen_type cjs_t
        |> Codegen.add_str ";"
  )
