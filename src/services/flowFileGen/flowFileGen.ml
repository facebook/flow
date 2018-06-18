(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


let spf = Printf.sprintf

let exports_map cx module_name =
  let module_map = Context.module_map cx in
  match SMap.get module_name module_map with
  | Some module_t -> (
    let module_t = Flow_js.resolve_type cx module_t in
    match Flow_js.Members.extract cx module_t with
    | Flow_js.Members.SuccessModule (named, cjs) -> (named, cjs)
    | _ -> failwith (
      spf "Failed to extract the exports of %s" (Type.string_of_ctor module_t)
    )
  )
  | None ->
    failwith (spf "Unable to extract %s from the module_map!" module_name)

let rec mark_declared_classes name t env = Codegen.(Type.(
  match resolve_type t env with
  | ThisClassT (_, DefT (_, InstanceT (_, _, _, {class_id; _;}))) ->
    set_class_name class_id name env
  | DefT (_, PolyT (_, t, _)) ->
    mark_declared_classes name t env
  | _ ->
    env
))

let gen_imports env =
  (**
   * Print import statements.
   *
   * TODO: For now we just print all import statements, but it would be nice to
   *       only print the ones that are actually used by the declares.
   *)
  let import_stmts = Context.import_stmts env.Codegen.flow_cx in
  let env = List.fold_left (fun env stmt ->
    let open Ast in
    let open Statement in
    let open ImportDeclaration in
    let {importKind; source; specifiers; default;} = stmt in
    let default = Option.map ~f:(fun (_, name) -> name) default in
    let named, ns = match specifiers with
      | Some (ImportNamespaceSpecifier (_, (_, name))) ->
        ([], Some name)
      | Some (ImportNamedSpecifiers xs) ->
        (xs, None)
      | None ->
        ([], None)
    in
    let _, { Ast.StringLiteral.value = source; _ } = source in

    let env = Codegen.add_str "import " env in
    let env =
      match importKind with
      | ImportType -> Codegen.add_str "type " env
      | ImportTypeof -> Codegen.add_str "typeof " env
      | ImportValue -> env
    in
    let env =
      match default with
      | Some default ->
        let env = Codegen.add_str default env in
        if ns <> None || List.length named > 0
        then Codegen.add_str ", " env
        else env
      | None -> env
    in
    let env =
      match ns with
      | Some ns ->
        Codegen.add_str "* as " env |> Codegen.add_str ns
      | None -> env
    in
    let env = if List.length named = 0 then env else (
      let env = Codegen.add_str "{" env in
      let env =
        Codegen.gen_separated_list named ", " (fun {local; remote; kind;} env ->
          let (_, remote) = remote in
          match local with
          | Some (_, local) when local <> remote ->
            let env =
              match kind with
              | Some ImportType -> Codegen.add_str "type " env
              | Some ImportTypeof -> Codegen.add_str "typeof " env
              | Some ImportValue | None -> env
            in
            Codegen.add_str remote env
              |> Codegen.add_str " as "
              |> Codegen.add_str local
          | Some _ | None -> Codegen.add_str remote env
        ) env
      in
      Codegen.add_str "}" env
    ) in
    Codegen.add_str " from \"" env
      |> Codegen.add_str source
      |> Codegen.add_str "\";\n"
  ) env import_stmts in

  (**
   * For each imported type, mark any imported class types so that they are not
   * re-declared.
   *)
  let imported_ts = Context.imported_ts env.Codegen.flow_cx in
  (imported_ts, SMap.fold mark_declared_classes imported_ts env)

let gen_class_body =
  let gen_field ~static field_name p env = Codegen.(Type.(
    (**
     * All classes have an implicit `static name: string` field on them.
     * No need to re-print this.
     *)
    let is_static_name_field = static && field_name = "name" && (
      match p with
      | Field (_, t, _) ->
        (match resolve_type t env with
        | DefT (_, StrT AnyLiteral) -> true
        | _ -> false)
      | _ -> false
    ) in

    let is_empty_constructor = not static && field_name = "constructor" && (
      match p with
      | Method (_, t) ->
        (match resolve_type t env with
        | DefT (_, FunT (_, _, { params; return_t; _ })) ->
          (params = []) && (
            match resolve_type return_t env with
            | DefT (_, VoidT) -> true
            | _ -> false
          )
        | _ -> false)
      | _ -> false
    ) in

    if is_static_name_field || is_empty_constructor
    then env
    else (
      add_str "  " env
        |> gen_if static (add_str "static ")
        |> gen_prop field_name p
        |> add_str ";\n"
    )
  )) in

  fun static fields methods env -> Codegen.(
    let static_fields = Type.(
      match static with
      | DefT (_, ObjT {props_tmap; _}) ->
        find_props props_tmap env
      | t -> failwith (
        spf
          "Internal Error: Unexpected class static type: %s"
          (string_of_ctor t)
      )
    ) in

    let static_fields_count = SMap.cardinal static_fields in
    let fields_count = SMap.cardinal fields in
    let methods_count = SMap.cardinal methods in
    let total_members_count =
      static_fields_count + fields_count + methods_count
    in

    let env = add_str " {" env in
    if total_members_count = 0 then add_str "}" env else (
      add_str "\n" env
        |> SMap.fold (gen_field ~static:true) static_fields
        |> add_str "\n"
        |> SMap.fold (gen_field ~static:false) fields
        |> SMap.fold (gen_field ~static:false) methods
        |> add_str "}"
  )
)

class unexported_class_visitor = object(self)
  inherit [Codegen.codegen_env * Type.TypeSet.t * ISet.t] Type_visitor.t as super

  method! tvar cx pole (env, seen, imported_classids) r id =
    let t = Codegen.resolve_type (Type.OpenT (r, id)) env in
    self#type_ cx pole (env, seen, imported_classids) t

  method! type_ cx pole (env, seen, imported_classids) t = Codegen.(Type.(
    if TypeSet.mem t seen then (env, seen, imported_classids) else (
      let seen = TypeSet.add t seen in
      match t with
      (* class_id = 0 is top of the inheritance chain *)
      | DefT (_, InstanceT (_, _, _, {class_id; _;}))
        when class_id = 0 || ISet.mem class_id imported_classids ->
        (env, seen, imported_classids)

      | DefT (r, InstanceT (static, extends, implements, {
          class_id;
          fields_tmap;
          methods_tmap;
          structural;
          _
        })) when not (has_class_name class_id env || Reason.is_lib_reason r) ->
        let class_name = next_class_name env in

        (**
         * Add to the list of declared classes *FIRST* to prevent inifite loops
         * on recursive references to this class from within itself.
         *)
        let env = set_class_name class_id class_name env in
        let (env, seen, imported_classids) = super#type_ cx pole (env, seen, imported_classids) t in

        let env = env
          |> add_str "declare "
          |> add_str (if structural then "interface " else "class ")
          |> add_str class_name
        in

        let env =
          match resolve_type extends env with
          | ObjProtoT _ -> env
          | DefT (_, ClassT t) when (
              match resolve_type t env with | ObjProtoT _ -> true | _ -> false
            ) -> env
          | ThisTypeAppT (_, extends, _, None) ->
            add_str " extends " env |> gen_type extends
          | ThisTypeAppT (_, extends, _, Some ts) ->
            add_str " extends " env
              |> gen_type extends
              |> add_str "<"
              |> gen_separated_list ts ", " gen_type
              |> add_str ">"
          | extends -> add_str " extends " env |> gen_type extends
        in

        let env = match implements with
        | [] -> env
        | ts -> env
          |> add_str " implements "
          |> gen_separated_list ts ", " gen_type
        in

        let fields = find_props fields_tmap env in
        let methods = find_props methods_tmap env in
        let env = gen_class_body static fields methods env |> add_str "\n" in
        (env, seen, imported_classids)

      | t -> super#type_ cx pole (env, seen, imported_classids) t
    )
  ))
end

let gen_local_classes =
  let visitor = new unexported_class_visitor in
  let gen_unexported_classes imported_classids _name t env =
    let (env, _, _) =
      visitor#type_
        env.Codegen.flow_cx
        Type.Neutral
        (env, Type.TypeSet.empty, imported_classids)
        t
    in
    env
  in

  fun named_exports cjs_export env ->
    let (imported_ts, env) = gen_imports env in

    (* Find and mark all the declared *exported* classes first *)
    let env = SMap.fold mark_declared_classes named_exports env in

    (**
     * Codegen any classes that are referenced but not exported. We're careful
     * to not codegen classes that are referenced but *imported* as well.
     *)
    let all_exports =
      match cjs_export with
      | None -> named_exports
      | Some cjs_t -> SMap.add "*CJS*" cjs_t named_exports
    in
    let rec fold_imported_classid _name t set = Type.(
      match Codegen.resolve_type t env with
      | ThisClassT (_, DefT (_, InstanceT (_, _, _, {class_id; _;}))) ->
        ISet.add class_id set
      | DefT (_, PolyT (_, t, _)) -> fold_imported_classid _name t set
      | _ -> set
    ) in
    let imported_classids =
      SMap.fold fold_imported_classid imported_ts ISet.empty
    in
    SMap.fold (gen_unexported_classes imported_classids) all_exports env

let gen_named_exports =
  let rec fold_named_export name t env = Codegen.(Type.(
    let env = (
      match resolve_type t env with
      | DefT (_, FunT (_static, _prototype, {
          params;
          rest_param;
          return_t;
          _;
        })) ->
        let env =
          if name = "default"
          then add_str "declare export default function" env
          else add_str "declare export function " env |> add_str name
        in
        gen_tparams_list env
          |> add_str "("
          |> gen_func_params params rest_param
          |> add_str "): "
          |> gen_type return_t
          |> add_str ";"

      | DefT (_, PolyT (tparams, t, _)) ->
        add_tparams tparams env |> fold_named_export name t

      | ThisClassT (_, DefT (_, InstanceT (static, super, implements, {
          fields_tmap;
          methods_tmap;
          has_unknown_react_mixins = _;
          structural;
          _;
        }))) ->
        let fields = Codegen.find_props fields_tmap env in
        let methods = Codegen.find_props methods_tmap env in
        let env = add_str "declare export " env in
        let env = add_str (
          if structural then "interface"
          else if name = "default" then "default class"
          else spf "class %s" name
        ) env in
        let env = gen_tparams_list env in
        let env =
          match Codegen.resolve_type super env with
          | ObjProtoT _ -> env
          | (ThisTypeAppT _) as t -> add_str " extends " env |> gen_type t
          | _ -> failwith (
            spf "Unexpected super type for class: %s" (string_of_ctor super)
          )
        in
        let env = match implements with
        | [] -> env
        | ts -> env
          |> add_str " implements "
          |> gen_separated_list ts ", " gen_type
        in
        gen_class_body static fields methods env

      | DefT (_, TypeT t) ->
        add_str "export type " env
          |> add_str name
          |> gen_tparams_list
          |> add_str " = "
          |> gen_type t
          |> add_str ";"

      | t ->
        let env =
          if name = "default"
          then add_str "declare export default " env
          else add_str "declare export var " env |> add_str name |> add_str ": "
        in
        gen_type t env |> add_str ";"
    ) in
    add_str "\n" env
  )) in
  SMap.fold fold_named_export

let gen_exports named_exports cjs_export env =
  match cjs_export with
  | None -> gen_named_exports named_exports env
  | Some cjs_t ->
    let type_exports = SMap.filter Type.(fun _name t ->
      let t = match t with OpenT _ -> Codegen.resolve_type t env | _ -> t in
      match t with
      | DefT (_, TypeT _) | DefT (_, PolyT (_, DefT (_, TypeT _), _)) -> true
      | _ -> false
    ) named_exports in
    gen_named_exports type_exports env
      |> Codegen.add_str "\ndeclare module.exports: "
      |> Codegen.gen_type cjs_t
      |> Codegen.add_str ";"

let flow_file cx =
  let module_ref = Context.module_ref cx in
  let (named_exports, cjs_export) = exports_map cx module_ref in
  (* Drop the loc *)
  let named_exports = SMap.map snd named_exports in

  Codegen.mk_env cx
    |> Codegen.add_str "// @flow\n\n"
    |> gen_local_classes named_exports cjs_export
    |> gen_exports named_exports cjs_export
    |> Codegen.to_string
