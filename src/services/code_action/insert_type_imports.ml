(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Insert_type_utils

type import_declaration = {
  import_kind: Ast.Statement.ImportDeclaration.import_kind;
  source: Loc.t * Loc.t Ast.StringLiteral.t;
  default: (Loc.t, Loc.t) Ast.Identifier.t option;
  (* we only import this kind*)
  named_specifier: (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.named_specifier option;
}

type use_mode =
  | ValueUseMode
  | TypeUseMode

module Modules = struct
  let paths =
    List.fold_left
      (fun acc (k, v) -> SMap.add k v acc)
      SMap.empty
      Hardcoded_module_fixes.files_to_modules

  (* Relativize module name if in the same folder, or use haste paths, or fail *)
  let resolve file module_name =
    match module_name with
    | Modulename.String s -> s
    | Modulename.Filename f ->
      let f = File_key.to_string f in
      let local_file = Filename.basename f in
      (* Use hardcoded fix if there is one *)
      if SMap.mem local_file paths then
        SMap.find local_file paths
      else
        let dep_folder = Filename.dirname f in
        let this_folder = Filename.dirname (File_key.to_string file) in
        (* remove .flow extension if there is one *)
        let local_file =
          if Filename.extension local_file = "flow" then
            Filename.chop_extension local_file
          else
            local_file
        in
        (* remove .js extension *)
        let local_file = Filename.chop_extension local_file in
        if dep_folder = this_folder then
          Filename.concat "./" local_file
        else
          let relative_dir = Files.relative_path this_folder dep_folder in
          Filename.concat relative_dir local_file
end

module AstHelper = struct
  let mk_import_stmt ~import_kind ~default ~remote_name ~local_name ~source =
    let dummy_loc = Loc.none in
    let remote_id = Flow_ast_utils.ident_of_source (dummy_loc, remote_name) in
    let local_id = Flow_ast_utils.ident_of_source (dummy_loc, local_name) in
    let raw_from_string (str : string) = Printf.sprintf "%S" str in
    let source =
      ( dummy_loc,
        { Ast.StringLiteral.value = source; raw = raw_from_string source; comments = None } )
    in
    let (default, named_specifier) =
      if default then
        (Some local_id, None)
      else
        let local =
          if local_id = remote_id then
            None
          else
            Some local_id
        in
        (None, Some { Ast.Statement.ImportDeclaration.kind = None; local; remote = remote_id })
    in
    { import_kind; source; default; named_specifier }

  let mk_import_declaration_kind use_mode =
    match use_mode with
    | ValueUseMode -> Ast.Statement.ImportDeclaration.ImportTypeof
    | TypeUseMode -> Ast.Statement.ImportDeclaration.ImportType
end

module ExportsHelper : sig
  type import_info = {
    import_kind: Ast.Statement.ImportDeclaration.import_kind;
    default: bool;
  }

  val resolve : use_mode -> ALoc.t -> string -> (import_info, Error.import_error) result
end = struct
  type import_info = {
    import_kind: Ast.Statement.ImportDeclaration.import_kind;
    default: bool;
  }

  open Type_sig
  open Type_sig_collections
  module P = Type_sig_pack

  (* NOTE The checks below are only based on the name. Ideally we'd also match
   * with def_loc as well. This was not available for every case originally,
   * when this was based on types-first 1.0 export info. *)

  let packed_ref_name type_sig ref =
    let { Packed_type_sig.Module.local_defs; remote_refs; _ } = type_sig in
    match ref with
    | P.LocalRef { index; _ } ->
      let def = Local_defs.get local_defs index in
      def_name def
    | P.RemoteRef { index; _ } ->
      let remote_ref = Remote_refs.get remote_refs index in
      P.remote_ref_name remote_ref
    | P.BuiltinRef { name; _ } -> name

  let from_cjs_module_exports import_kind type_sig exports remote_name =
    match exports with
    (* module.exports = C; *)
    (* module.exports = class C ...; *)
    | Some (P.Ref ref) when packed_ref_name type_sig ref = remote_name ->
      Some { import_kind; default = true }
    (* module.exports = (new C(): C) *)
    | Some (P.TyRef (P.Unqualified ref)) when packed_ref_name type_sig ref = remote_name ->
      (* This is the only way we can import this. *)
      let import_kind = Ast.Statement.ImportDeclaration.ImportTypeof in
      Some { import_kind; default = true }
    (* module.exports = { C, ... } *)
    | Some (P.Value (ObjLit { props; _ })) when SMap.mem remote_name props ->
      Some { import_kind; default = false }
    | _ -> None

  let from_export_keys import_kind keys remote_name =
    if Array.exists (String.equal remote_name) keys then
      Some { import_kind; default = false }
    else
      None

  let from_type_sig import_kind type_sig remote_name =
    let { Packed_type_sig.Module.module_kind; _ } = type_sig in
    match module_kind with
    | P.CJSModule { exports; info = P.CJSModuleInfo { type_export_keys; _ }; _ } ->
      Utils_js.lazy_seq
        [
          lazy (from_cjs_module_exports import_kind type_sig exports remote_name);
          lazy (from_export_keys import_kind type_export_keys remote_name);
        ]
    | P.ESModule { info = P.ESModuleInfo { export_keys; type_export_keys; _ }; _ } ->
      Utils_js.lazy_seq
        [
          lazy (from_export_keys import_kind export_keys remote_name);
          lazy (from_export_keys import_kind type_export_keys remote_name);
        ]

  (* NOTE Here we assume 'react' is available as a library *)
  let from_react loc =
    if is_react_loc loc then
      let import_kind = Ast.Statement.ImportDeclaration.ImportType in
      Some { import_kind; default = false }
    else
      None

  let from_react_redux loc =
    if is_react_redux_loc loc then
      let import_kind = Ast.Statement.ImportDeclaration.ImportType in
      Some { import_kind; default = false }
    else
      None

  (* Try to find out whether a given symbol is exported and what kind of import
   * we need to use for it. *)
  let resolve use_mode loc name =
    match ALoc.source loc with
    | None -> Error Error.Loc_source_none
    | Some remote_file ->
      (match Parsing_heaps.Reader.get_type_sig ~reader:(State_reader.create ()) remote_file with
      | None -> Error Error.Parsing_heaps_get_sig_error
      | Some type_sig ->
        let import_kind = AstHelper.mk_import_declaration_kind use_mode in
        let import_info_opt =
          Utils_js.lazy_seq
            [
              lazy (from_type_sig import_kind type_sig name);
              lazy (from_react loc);
              lazy (from_react_redux loc);
            ]
        in
        (match import_info_opt with
        | None -> Error (Error.No_matching_export (name, loc))
        | Some import_info -> Ok import_info))
end

module ImportsHelper : sig
  (* This class is used to convert Remote symbols appearing in annotated types
   * to Imported symbols. A side effect of this process is the generation of
   * import statements. At the end of the AST traversal these imports will be
   * gathered and prepended to the file.
   *)
  class remote_converter :
    iteration:int
    -> file:File_key.t
    -> reserved_names:SSet.t
    -> object
         method type_ : Ty.t -> (Ty.t, Error.kind) result

         method to_import_stmts : unit -> (Loc.t, Loc.t) Ast.Statement.t list

         method to_import_bindings : (string * Autofix_imports.bindings) list
       end

  val imports_react : File_sig.With_ALoc.t -> bool
end = struct
  (* A structure holding information about the import of a Remote symbol.
   *
   * Assume a name "foo" needs to be imported:
   *
   *  - as "type" from a.js, b.js, and
   *  - as "typeof" from a.js, c.js
   *
   * The corresponding records will be:
   *
   *  { index = 0; use_mode: Type ; remote: Symbol(foo, a.js); }
   *  { index = 1; use_mode: Type ; remote: Symbol(foo, b.js); }
   *  { index = 2; use_mode: Value; remote: Symbol(foo, a.js); }
   *  { index = 3; use_mode: Value; remote: Symbol(foo, c.js); }
   *
   * Index is used to distinguish imports of the same name from different sources.
   *
   * An entry of the form:
   *
   *   { index = N; use_mode: Type; remote: Symbol(name, source); }
   *
   * corresponds to the name:
   *
   *   $IMPORTED_TYPE$name   if N = 0
   *   $IMPORTED_TYPE$name_N if N > 0
   *
   * And an entry of the form:
   *
   *   { index = N; use_mode: Value; remote: Symbol(name, source); }
   *
   * corresponds to the name:
   *
   *   $IMPORTED_TYPEOF$name   if N = 0
   *   $IMPORTED_TYPEOF$name_N if N > 0
   *
   * The import statements for the entries:
   *
   *   { index = 0; use_mode: Value; remote: Symbol('foo', 'a.js'); }
   *   { index = 1; use_mode: Value; remote: Symbol('foo', 'b.js'); }
   *   { index = 2; use_mode: Type ; remote: Symbol('foo', 'a.js'); }
   *
   * will be, respectively:
   *
   *   import typeof { foo as $IMPORTED_TYPEOF$foo   } from 'a';
   *   import typeof { foo as $IMPORTED_TYPEOF$foo_1 } from 'b';
   *   import type   { foo as $IMPORTED_TYPE$foo_2   } from 'a';
   *
   *)
  module ImportInfo = struct
    type t = {
      index: int;
      use_mode: use_mode;
      remote: Ty.symbol;
      import_declaration: import_declaration;
    }

    let to_local_name ~iteration ~reserved_names index use_mode name =
      (* If the name does not appear in the program and this is the first occurence
       * that we are introducing and in the first iteration, then it's fine to reuse
       * the original import name. *)
      if (not (SSet.mem name reserved_names)) && index = 0 && iteration = 0 then
        name
      else
        let prefix =
          match use_mode with
          | ValueUseMode -> "TYPEOF"
          | TypeUseMode -> "TYPE"
        in
        let iteration =
          match iteration with
          | 0 -> ""
          | i -> Utils_js.spf "%d$" i
        in
        let index =
          match index with
          | 0 -> ""
          | _ -> Utils_js.spf "_%d" index
        in
        Utils_js.spf "$IMPORTED_%s$%s_%s%s" prefix iteration name index

    let to_local_symbol ~iteration ~reserved_names x =
      let { index; use_mode; _ } = x in
      match x.remote with
      | {
       Ty.sym_provenance = Ty.Remote { Ty.imported_as = _ };
       sym_anonymous = false;
       sym_def_loc;
       sym_name;
      } ->
        let local_name =
          to_local_name
            ~iteration
            ~reserved_names
            index
            use_mode
            (Reason.display_string_of_name sym_name)
        in
        let import_mode =
          match use_mode with
          | ValueUseMode -> Ty.TypeofMode
          | TypeUseMode -> Ty.TypeMode
        in
        {
          Ty.sym_provenance =
            Ty.Remote { Ty.imported_as = Some (ALoc.none, local_name, import_mode) };
          sym_anonymous = false;
          sym_name = Reason.OrdinaryName local_name;
          sym_def_loc;
        }
      | {
       Ty.sym_provenance = Ty.Library { Ty.imported_as = _ };
       sym_anonymous = false;
       sym_def_loc;
       sym_name;
      }
      (* Special-case react-redux. *)
        when is_react_redux_loc sym_def_loc ->
        let local_name =
          to_local_name
            ~iteration
            ~reserved_names
            index
            use_mode
            (Reason.display_string_of_name sym_name)
        in
        let import_mode =
          match use_mode with
          | ValueUseMode -> Ty.TypeofMode
          | TypeUseMode -> Ty.TypeMode
        in
        {
          Ty.sym_provenance =
            Ty.Library { Ty.imported_as = Some (ALoc.none, local_name, import_mode) };
          sym_anonymous = false;
          sym_name = Reason.OrdinaryName local_name;
          sym_def_loc;
        }
      | s -> s

    (* debug *)
    let dump x =
      let { index; use_mode; remote; _ } = x in
      let use_mode_str =
        match use_mode with
        | ValueUseMode -> "value"
        | TypeUseMode -> "type"
      in
      Utils_js.spf
        "(symbol: '%s', use_mode: %s, index: %d)"
        (Ty_debug.dump_symbol remote)
        use_mode_str
        index
  end

  (* Mapping from imported names to lists of imports (ImportInfo). Here we hold
   * information of symbols that have been successfully imported. Only the symbols
   * that appear here will be used to add the necessary imports at the end of
   * the tranformation.
   *)
  module ImportedNameMap : sig
    type t

    val empty : t

    val add : ImportInfo.t -> t -> t

    val next_index : Ty.symbol -> use_mode -> t -> int

    val fold : (ImportInfo.t -> 'a -> 'a) -> t -> 'a -> 'a
  end = struct
    type t = ImportInfo.t Nel.t NameUtils.Map.t

    let empty = NameUtils.Map.empty

    let add info m =
      let name = info.ImportInfo.remote.Ty.sym_name in
      let lst' =
        match NameUtils.Map.find_opt name m with
        | Some lst -> Nel.cons info lst
        | None -> Nel.one info
      in
      NameUtils.Map.add name lst' m

    let next_index remote_symbol mode x =
      let { Ty.sym_name; _ } = remote_symbol in
      match NameUtils.Map.find_opt sym_name x with
      | None -> 0
      | Some lst ->
        let max_index =
          Nel.fold_left
            (fun acc { ImportInfo.index; use_mode; _ } ->
              if use_mode = mode then
                max acc index
              else
                acc)
            0
            lst
        in
        max_index + 1

    let fold f x acc =
      NameUtils.Map.fold (fun _ lst a -> Nel.fold_left (fun b y -> f y b) a lst) x acc

    (* debug *)
    let _dump m =
      NameUtils.Map.bindings m
      |> List.map (fun (k, v) ->
             Utils_js.spf
               "'%s' ->\n%s\n"
               (Reason.display_string_of_name k)
               (Nel.map ImportInfo.dump v |> Nel.to_list |> String.concat "\n"))
      |> String.concat "\n"
  end

  module SymbolWithUseModeMap = WrappedMap.Make (struct
    type t = Ty.symbol * use_mode

    let compare = Stdlib.compare
  end)

  (* Any errors we encounter should surface as an error to the top-level type
   * that is converted. This exception needs to be caught in 'type_'. *)
  exception Import_exc of Error.import_error

  module BatchImportMap = WrappedMap.Make (struct
    open Ast

    type t = Statement.ImportDeclaration.import_kind * (Loc.t * Loc.t StringLiteral.t)

    let compare = Stdlib.compare
  end)

  module BatchImportBindingsMap = WrappedMap.Make (struct
    type t = Ast.Statement.ImportDeclaration.import_kind * string

    let compare = Stdlib.compare
  end)

  class remote_converter ~iteration ~file ~reserved_names =
    object (self)
      val mutable name_map = ImportedNameMap.empty

      val mutable symbol_cache = SymbolWithUseModeMap.empty

      method private gen_import_stmt index use_mode remote_symbol
          : (import_declaration, Error.import_error) result =
        let reader = State_reader.create () in
        let { Ty.sym_name = remote_name; sym_def_loc; _ } = remote_symbol in
        let module_name =
          match ALoc.source sym_def_loc with
          | Some remote_source ->
            (* [LIB] react.js exports a number of modules so imports from there
             * will fail if we follow the general path. Instead assume it is okay to
             * use `import type { ... } from 'react'` and hard-code the module string
             * here. *)
            if is_react_file_key remote_source then
              Modulename.String "react"
            else if is_react_redux_file_key remote_source then
              Modulename.String "react-redux"
            else
              let info =
                Module_heaps.Reader.get_info_unsafe ~reader ~audit:Expensive.warn remote_source
              in
              info.Module_heaps.module_name
          | None -> failwith "No source"
        in
        (* TODO we should probably give up if we are trying to generate an import statement with
         * an internal name. However, to avoid a behavior change let's do the conversion for now. *)
        let remote_name = Reason.display_string_of_name remote_name in
        ExportsHelper.resolve use_mode sym_def_loc remote_name
        >>| fun { ExportsHelper.import_kind; default } ->
        let source = Modules.resolve file module_name in
        let local_name =
          ImportInfo.to_local_name ~iteration ~reserved_names index use_mode remote_name
        in
        AstHelper.mk_import_stmt ~import_kind ~default ~remote_name ~local_name ~source

      method private create_symbol use_mode remote_symbol =
        let index = ImportedNameMap.next_index remote_symbol use_mode name_map in
        let info_result =
          self#gen_import_stmt index use_mode remote_symbol >>| fun import_declaration ->
          { ImportInfo.index; use_mode; remote = remote_symbol; import_declaration }
        in
        symbol_cache <- SymbolWithUseModeMap.add (remote_symbol, use_mode) info_result symbol_cache;
        name_map <-
          begin
            match info_result with
            | Ok info -> ImportedNameMap.add info name_map
            | Error _ -> name_map
          end;
        info_result

      method private convert_symbol use_mode remote_symbol =
        let result =
          match SymbolWithUseModeMap.find_opt (remote_symbol, use_mode) symbol_cache with
          | None -> self#create_symbol use_mode remote_symbol
          | Some info -> info
        in
        match result with
        | Ok info -> ImportInfo.to_local_symbol ~iteration ~reserved_names info
        | Error err -> raise (Import_exc err)

      method type_ =
        let covert_ty_visitor =
          object
            inherit [_] Ty.endo_ty as super

            method! on_t env t =
              match t with
              | Ty.TypeOf
                  (Ty.TSymbol
                    ( {
                        Ty.sym_provenance =
                          Ty.Remote { Ty.imported_as = None | Some (_, _, Ty.TypeMode) };
                        sym_anonymous = false;
                        _;
                      } as s )) ->
                let local = self#convert_symbol ValueUseMode s in
                Ty.Generic (local, Ty.ClassKind, None)
              | Ty.TypeOf
                  (Ty.TSymbol
                    {
                      Ty.sym_provenance =
                        Ty.Remote { Ty.imported_as = Some (sym_def_loc, sym_name, Ty.TypeofMode) };
                      sym_anonymous = false;
                      _;
                    }) ->
                let local =
                  {
                    Ty.sym_provenance = Ty.Local;
                    sym_def_loc;
                    sym_name = Reason.OrdinaryName sym_name;
                    sym_anonymous = false;
                  }
                in
                Ty.Generic (local, Ty.ClassKind, None)
              | _ -> super#on_t env t

            method! on_symbol env s =
              match s with
              | {
               Ty.sym_provenance = Ty.Remote { Ty.imported_as = None };
               sym_anonymous = false;
               _;
              } ->
                self#convert_symbol TypeUseMode s
              | {
               Ty.sym_provenance = Ty.Remote { Ty.imported_as = Some (_, local_name, _) };
               sym_anonymous = false;
               sym_def_loc;
               _;
              } ->
                {
                  Ty.sym_provenance = Ty.Local;
                  sym_anonymous = false;
                  sym_name = Reason.OrdinaryName local_name;
                  sym_def_loc;
                }
              (* react-redux *)
              | {
               Ty.sym_provenance = Ty.Library { Ty.imported_as = None };
               sym_anonymous = false;
               sym_def_loc;
               _;
              }
                when is_react_redux_loc sym_def_loc ->
                self#convert_symbol TypeUseMode s
              | {
               Ty.sym_provenance = Ty.Library { Ty.imported_as = Some (_, local_name, _) };
               sym_anonymous = false;
               sym_def_loc;
               _;
              }
                when is_react_redux_loc sym_def_loc ->
                {
                  Ty.sym_provenance = Ty.Local;
                  sym_anonymous = false;
                  sym_name = Reason.OrdinaryName local_name;
                  sym_def_loc;
                }
              | _ -> super#on_symbol env s
          end
        in
        fun ty ->
          ( let old_name_map = name_map in
            let old_symbol_cache = symbol_cache in
            match covert_ty_visitor#on_t () ty with
            | exception Import_exc err ->
              (* NOTE If an error occurs, reset the imported symbols (name_map),
               * but keep the cache to avoid duplicating work later. *)
              name_map <- old_name_map;
              symbol_cache <- old_symbol_cache;
              Error (Error.Import_error err)
            | ty' -> Ok ty'
            : (Ty.t, Error.kind) result )

      method to_import_stmts () : (Loc.t, Loc.t) Ast.Statement.t list =
        let dummy_loc = Loc.none in
        let (default_list, named_map) =
          ImportedNameMap.fold
            (fun { ImportInfo.import_declaration; _ } (default_acc, named_acc) ->
              let { import_kind; source; default; named_specifier } = import_declaration in
              match (default, named_specifier) with
              | (None, Some named_specifier) ->
                let named_specifiers =
                  match BatchImportMap.find_opt (import_kind, source) named_acc with
                  | Some named_specifiers -> named_specifier :: named_specifiers
                  | None -> [named_specifier]
                in
                (default_acc, BatchImportMap.add (import_kind, source) named_specifiers named_acc)
              | _ -> (import_declaration :: default_acc, named_acc))
            name_map
            ([], BatchImportMap.empty)
        in
        let import_stmts =
          List.map
            (fun import_declaration ->
              let { import_kind; source; default; named_specifier } = import_declaration in
              let specifiers =
                match named_specifier with
                | Some specifer ->
                  Some (Ast.Statement.ImportDeclaration.ImportNamedSpecifiers [specifer])
                | None -> None
              in
              ( dummy_loc,
                Ast.Statement.ImportDeclaration
                  {
                    Ast.Statement.ImportDeclaration.import_kind;
                    source;
                    default;
                    specifiers;
                    comments = None;
                  } ))
            default_list
        in
        let import_stmts =
          BatchImportMap.fold
            (fun (import_kind, source) named_specifiers acc ->
              let specifiers =
                Some (Ast.Statement.ImportDeclaration.ImportNamedSpecifiers named_specifiers)
              in
              let import_stmt =
                ( dummy_loc,
                  Ast.Statement.ImportDeclaration
                    {
                      Ast.Statement.ImportDeclaration.import_kind;
                      source;
                      default = None;
                      specifiers;
                      comments = None;
                    } )
              in
              import_stmt :: acc)
            named_map
            import_stmts
        in
        import_stmts

      method to_import_bindings : (string * Autofix_imports.bindings) list =
        let (bindings, named_map) =
          let open Ast.Identifier in
          ImportedNameMap.fold
            (fun {
                   ImportInfo.import_declaration =
                     {
                       import_kind;
                       source = (_, { Ast.StringLiteral.value = from; _ });
                       default;
                       named_specifier;
                     };
                   _;
                 }
                 (default_acc, named_acc) ->
              match (default, named_specifier) with
              | ( None,
                  Some
                    {
                      Ast.Statement.ImportDeclaration.remote = (_, { name = remote_name; _ });
                      local;
                      _;
                    } ) ->
                let named_binding =
                  {
                    Autofix_imports.remote_name;
                    local_name =
                      (match local with
                      | None -> None
                      | Some (_, { name; _ }) -> Some name);
                  }
                in
                let named_bindings =
                  match BatchImportBindingsMap.find_opt (import_kind, from) named_acc with
                  | Some named_bindings -> named_binding :: named_bindings
                  | None -> [named_binding]
                in
                ( default_acc,
                  BatchImportBindingsMap.add (import_kind, from) named_bindings named_acc )
              | (Some (_, { name; _ }), _) ->
                ((from, Autofix_imports.Default name) :: default_acc, named_acc)
              | _ -> (default_acc, named_acc))
            name_map
            ([], BatchImportBindingsMap.empty)
        in
        let bindings =
          BatchImportBindingsMap.fold
            (fun (import_kind, from) named_bindings acc ->
              match import_kind with
              | Ast.Statement.ImportDeclaration.ImportType ->
                (from, Autofix_imports.NamedType named_bindings) :: acc
              | _ -> (from, Autofix_imports.Named named_bindings) :: acc)
            named_map
            bindings
        in
        bindings
    end

  exception Found_react_import

  let imports_react =
    File_sig.With_ALoc.(
      let from_binding binding =
        match binding with
        | BindIdent (_, "React") -> raise Found_react_import
        | BindIdent _
        | BindNamed _ ->
          ()
        (* React should be top-level, not destructured *)
      in
      let from_bindings bindings_opt = Base.Option.iter ~f:from_binding bindings_opt in
      let from_require require =
        match require with
        | Require { source = _; require_loc = _; bindings } -> from_bindings bindings
        | Import { ns = Some (_, "React"); _ } -> raise Found_react_import
        | Import _
        | ImportDynamic _
        | Import0 _ ->
          ()
      in
      let from_requires requires = List.iter from_require requires in
      fun file_sig ->
        let requires = file_sig.module_sig.requires in
        match from_requires requires with
        | exception Found_react_import -> true
        | _ -> false)
end
