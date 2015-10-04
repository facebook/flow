(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module is the entry point of the typechecker. It sets up subtyping
   constraints for every expression, statement, and declaration form in a
   JavaScript AST; the subtyping constraints are themselves solved in module
   Flow_js. It also manages environments, including not only the maintenance of
   scope information for every function (pushing/popping scopes, looking up
   variables) but also flow-sensitive information about local variables at every
   point inside a function (and when to narrow or widen their types). *)

open Utils
open Sys_utils

module Ast = Spider_monkey_ast
module Constraint = Constraint_js
module Flow = Flow_js
module Modes = Modes_js
module Reason = Reason_js
module ErrorSet = Errors_js.ErrorSet

type info = {
  file: string;             (* file name *)
  _module: string;          (* module name *)
  required: SSet.t;         (* required module names *)
  require_loc: Spider_monkey_ast.Loc.t SMap.t;  (* statement locations *)
  strict_required: SSet.t;  (* strict requires (flow to export types) *)
  checked: bool;            (* in flow? *)
}

type mode = ModuleMode_Checked | ModuleMode_Weak | ModuleMode_Unchecked

(****************** shared dependency map *********************)

(* map from module name to filename *)
module NameHeap = SharedMem.WithCache (String) (struct
  type t = string
  let prefix = Prefix.make()
end)

(* map from file name to module info *)
(* note: currently we may have many files for one module name.
   this is an issue. *)
module InfoHeap = SharedMem.WithCache (String) (struct
  type t = info
  let prefix = Prefix.make()
end)

(****************** header parse utils *********************)

(* scan the first node in an AST comment list, using the given
   word-list parser and default result. default is returned if
   parser returns nothing, or comment list is empty.
   Note: 'header' is a bit of a misnomer. we simply call this
   on the comment list returned from the parser, which makes no
   guarantees that the first comment appears before anything else *)
let parse_header wordlist_parser default =
  let words = Str.regexp "[ \t\n\\*/]+"
  in Ast.Comment.(function
    | [] -> default
    | (loc, Block s) :: _
    | (loc, Line s) :: _ ->
        match wordlist_parser (Str.split words s) with
        | Some x -> x
        | None -> default
  )

(****************** @flow parser *********************)

let rec parse_attributes_flow = function
  | "@flow" :: "weak" :: _ -> Some ModuleMode_Weak
  | "@flow" :: _ -> Some ModuleMode_Checked
  | _ :: xs -> parse_attributes_flow xs
  | [] -> None

let parse_flow = parse_header parse_attributes_flow ModuleMode_Unchecked

(** module systems **)

(* shared heap for package.json tokens by filename *)
module PackageHeap = SharedMem.WithCache (String) (struct
    type t = Spider_monkey_ast.Expression.t SMap.t
    let prefix = Prefix.make()
  end)

(* shared heap for package.json directories by package name *)
module ReversePackageHeap = SharedMem.WithCache (String) (struct
    type t = string
    let prefix = Prefix.make()
  end)

let get_key key tokens = Spider_monkey_ast.(
  match SMap.get (spf "\"%s\"" key) tokens with
  | Some (_, Expression.Literal { Literal.value = Literal.String name; _ }) ->
      Some name
  | _ -> None
)

(* basic parse of JSON as a JS expression; throws on parse failure *)
let parse_json json file =
  let js = spf "(%s)" json in
  let ast, errors = Parsing_service_js.do_parse ~keep_errors:true js file in
  Spider_monkey_ast.(match ast with
    | Some (_, [_, Statement.Expression ({
        Statement.Expression.expression = _, Expression.Object {
          Expression.Object.properties = ps
        }
      })], _) ->
        List.fold_left Expression.Object.(fun map -> function
          | Property (_, {
              Property.key = Property.Literal (_, { Literal.raw; _ });
              value;
              _
            }) -> SMap.add raw value map
          | _ -> map
        ) SMap.empty ps
    | _ -> SMap.empty
  ), errors

let add_package package =
  let json = cat package in
  let tokens, errors = parse_json json package in
  PackageHeap.add package tokens;
  (match get_key "name" tokens with
  | Some name ->
    ReversePackageHeap.add name (Filename.dirname package)
  | None -> ());
  errors

(* Specification of a module system. Currently this signature is sufficient to
   model both Haste and Node, but should be further generalized. *)
module type MODULE_SYSTEM = sig
  (* Given a file and comments in it, make the name of the module it exports. *)
  val exported_module: string -> Spider_monkey_ast.Comment.t list -> string

  (* Given a file and a reference in it to an imported module, make the name of
     the module it refers to. *)
  val imported_module: string -> string -> string

(* for a given module name, choose a provider from among a set of
   files with that exported name. also check for duplicates and
   generate warnings, as dictated by module system rules. *)
val choose_provider:
  string ->   (* module name *)
  SSet.t ->   (* set of candidate provider files *)
  (* parallel lists of error files and error sets (accumulator) *)
  (string list * ErrorSet.t list) ->
  (* file, parallel lists of error files and error sets (additive) *)
  (string * (string list * ErrorSet.t list))

end

(****************** Node module system *********************)

let seq f g =
  match f () with
  | Some x -> Some x
  | None -> g ()

let rec seqf f = function
  | x :: xs ->
    (match f x with
    | Some v -> Some v
    | None -> seqf f xs)
  | [] -> None

module Node = struct
  let exported_module file comments = file

  let path_if_exists path =
    if Sys.file_exists path then Some path
    else None

  let path_is_file path =
    Sys.file_exists path && not (Sys.is_directory path)

  let parse_main package =
    if not (Sys.file_exists package) then None
    else
      let tokens = package |> PackageHeap.find_unsafe in
      let dir = Filename.dirname package in
      match get_key "main" tokens with
      | None -> None
      | Some file ->
          let path = Files_js.normalize_path dir file in
          if path_is_file path
          then Some path
          else seq
            (fun () ->
              seqf
                (fun ext -> path_if_exists (path ^ ext))
                Files_js.flow_extensions)
            (fun () ->
              let path = Filename.concat path "index.js" in
              path_if_exists path)

  let resolve_relative dir r =
    let path = Files_js.normalize_path dir r in
    if Files_js.is_flow_file path && path_is_file path
    then path_if_exists path
    else seq
      (fun () ->
        seqf
          (fun ext -> path_if_exists (path ^ ext))
          Files_js.flow_extensions
      )
      (fun () -> seq
          (fun () ->
            let package = Filename.concat path "package.json" in
            parse_main package)
          (fun () ->
            let path = Filename.concat path "index.js" in
            path_if_exists path
          )
      )

  let rec node_module dir r = seq
    (fun () -> resolve_relative dir (spf "node_modules%s%s" Filename.dir_sep r))
    (fun () ->
      let parent_dir = Filename.dirname dir in
      if dir = parent_dir then None
      else node_module (Filename.dirname dir) r
    )

  let relative r =
    Str.string_match Files_js.dir_sep r 0
    || Str.string_match Files_js.current_dir_name r 0
    || Str.string_match Files_js.parent_dir_name r 0

  let resolve_import file r =
    let dir = Filename.dirname file in
    if relative r
    then resolve_relative dir r
    else node_module dir r

  let imported_module file r =
    let opt_module_name = resolve_import file r in
    match opt_module_name with
    | Some r -> r
    | _ -> r

(* in node, file names are module names, as guaranteed by
   our implementation of exported_name, so anything but a
   singleton provider set is craziness. *)
let choose_provider m files errs =
  match SSet.elements files with
  | [] ->
      failwith (spf "internal error: empty provider set for module %S" m)
  | [f] ->
      f, errs
  | files ->
      failwith (spf "internal error: multiple providers for module %S" m)

end

(****************** Haste module system *********************)

module Haste: MODULE_SYSTEM = struct
  let module_name_of file =
    try Filename.chop_extension file
    (* TODO: Think of what we should do when we got a filename without
     an extension *)
    with _ -> file

  let short_module_name_of file =
    Filename.basename file |> Filename.chop_extension

  let rec parse_attributes_module_name = function
    | "@providesModule" :: m :: _ -> Some m
    | _ :: xs -> parse_attributes_module_name xs
    | [] -> None

  let parse_module_name = parse_header parse_attributes_module_name

  let is_mock =
    let mock_path = Str.regexp ".*/__mocks__/.*" in
    fun file -> Str.string_match mock_path file 0

  let exported_module file comments =
    if is_mock file
    then short_module_name_of file
    else
      parse_module_name file comments

  let expanded_name r =
    match Str.split_delim Files_js.dir_sep r with
    | [] -> None
    | package_name::rest ->
        ReversePackageHeap.get package_name |> opt_map (fun package ->
          Files_js.construct_path package rest
        )

  (* similar to Node resolution, with possible special cases *)
  let resolve_import file r =
    seq
      (fun () -> Node.resolve_import file r)
      (fun () ->
        match expanded_name r with
        | Some r -> Node.resolve_relative (Filename.dirname file) r
        | None -> None
      )

  let imported_module file r =
    match resolve_import file r with
    | Some r -> r
    | None -> Haste_module_preprocessor.preprocess_name r

(* in haste, many files may provide the same module. here we're also
   supporting the notion of mock modules - allowed duplicates used as
   fallbacks. we prefer the non-mock if it exists, otherwise choose an
   arbitrary mock, if any exist. if multiple non-mock providers exist,
   we pick one arbitrarily and issue duplicate module warnings for the
   rest. *)
let choose_provider m files (errfiles, errsets) =
  match SSet.elements files with
  | [] ->
      failwith (spf "internal error: empty provider set for module %S" m)
  | [f] ->
      f, (errfiles, errsets)
  | files ->
      let mocks, non_mocks = List.partition is_mock files in
      match non_mocks with
      | [] -> List.hd mocks, (errfiles, errsets)
      | [f] -> f, (errfiles, errsets)
      | h :: t ->
          let errfiles, errsets = List.fold_left (fun (errfiles, errsets) f ->
            let w = Flow.new_warning [
              Reason.new_reason m (Pos.make_from
                (Relative_path.create Relative_path.Dummy f)),
                "Duplicate module provider";
              Reason.new_reason "current provider"
                (Pos.make_from (Relative_path.create Relative_path.Dummy h)),
                ""] in
            f :: errfiles, (ErrorSet.singleton w) :: errsets
          ) (errfiles, errsets) t in
          h, (errfiles, errsets)

end

(****************** module system switch *********************)

(* Switch between module systems, based on environment. We could eventually use
   functors, but that seems like overkill at this point. *)

let module_system_table =
  let table = Hashtbl.create 2 in
  Hashtbl.add table "node" (module Node: MODULE_SYSTEM);
  Hashtbl.add table "haste" (module Haste: MODULE_SYSTEM);
  table

let module_system = ref (module Node: MODULE_SYSTEM)

let init specifier =
  module_system := Hashtbl.find module_system_table specifier

let exported_module file comments =
  let module M = (val !module_system) in
  M.exported_module file comments

let imported_module file r =
  let module M = (val !module_system) in
  M.imported_module file r

let choose_provider m files errs =
  let module M = (val !module_system) in
  M.choose_provider m files errs

(****************** reverse import utils *********************)

(* map from module name to modules which import it. *)
(* note: this is outside InfoHeap because InfoHeap gets changed concurrently
   by the infer jobs, we cannot simply build the information up there *)
let reverse_imports_map = Hashtbl.create 0

let reverse_imports_clear module_name =
  Hashtbl.remove reverse_imports_map module_name

let reverse_imports_get module_name =
  try Some (Hashtbl.find reverse_imports_map module_name)
  with Not_found -> None

let reverse_imports_track importer_name requires =
  let add_requires module_name =
    let reverse_imports = match reverse_imports_get module_name with
    | Some reverse_imports ->
        reverse_imports
    | None ->
        SSet.empty
    in
    let new_reverse_imports = SSet.add importer_name reverse_imports in
    Hashtbl.replace reverse_imports_map module_name new_reverse_imports
  in
  SSet.iter add_requires requires

let reverse_imports_untrack importer_name requires =
  let remove_requires module_name =
    match reverse_imports_get module_name with
    | Some reverse_imports ->
        let new_reverse_imports = SSet.remove importer_name reverse_imports in
        if not (SSet.is_empty new_reverse_imports)
        then Hashtbl.replace reverse_imports_map module_name new_reverse_imports
        else begin
          (* in case the reverse import map is empty we might have hit a case
             where the module we keep track of exists but is not imported by
             anything anymore. in this case we do not clear the tracking
             information but simply set it to empty. this makes sure that
             the information returned by get-imported-by is never confusing
             for exisitng, but not imported modules *)
          if NameHeap.mem module_name
          then Hashtbl.replace reverse_imports_map module_name SSet.empty
          else reverse_imports_clear module_name
        end
    | None ->
        ()
  in SSet.iter remove_requires requires

(******************)
(***** public *****)
(******************)

let module_exists = NameHeap.mem

let get_file m =
  match NameHeap.get m with
  | Some file -> file
  | None -> failwith
      (spf "file name not found for module %s" m)

(* Gets the filename for a module without failing when it does not exist *)
let get_module_file m =
  NameHeap.get m

let get_module_info f =
  match InfoHeap.get f with
  | Some info -> info
  | None -> failwith
      (spf "module info not found for file %s" f)

let get_module_name f =
  (get_module_info f)._module

let add_reverse_imports filenames =
  List.iter (fun filename ->
    let { _module = name; required = req; _ } = get_module_info filename in
    (* we only add requriements from actual module providers. this avoids
       strange states when two files provide the same module. *)
    match get_module_file name with
    | Some file when file = filename ->
        (* we need to make sure we are in the reverse import heap to avoid
           confusing behavior when querying it *)
        (if not (Hashtbl.mem reverse_imports_map name)
        then Hashtbl.add reverse_imports_map name SSet.empty);
        reverse_imports_track name req
    | _ -> ()
  ) filenames

let get_reverse_imports module_name =
  reverse_imports_get module_name

(* extract info from context *)
let info_of cx = {
  file = cx.Constraint.file;
  _module = cx.Constraint._module;
  required = cx.Constraint.required;
  require_loc = cx.Constraint.require_loc;
  strict_required = cx.Constraint.strict_required;
  checked = cx.Constraint.checked
}

(* during inference, we add per-file module info to the shared heap
   from worker processes.
   Note that we wait to choose providers until inference is complete. *)
let add_module_info cx =
  let info = info_of cx in
  InfoHeap.add info.file info

(* Note that the module provided by a file is always accessible via its full
   path, so that it may be imported by specifying (a part of) that path in any
   module system. So, e.g., a file whose full path is /foo/bar.js is considered
   to export a module by that name, so that it is always possible to import it
   from any other file in the file system by using a relative path or some other
   file system navigation convention. The Node module system relies on this
   basic setup.

   In addition, a file may or may not export its module by another name: this
   name is typically shorter, but can be used unambiguously throughout the file
   system to import the module, and this access mechanism is somewhat robust to
   moving files around in the file system. Thus, /foo/bar.js may also export its
   module by the name Bar, using a custom module system like Haste, either
   explicitly (by mentioning Bar in the file) or implicitly (following some
   convention), so that it can be imported from any other file in the file
   system by the name Bar. Other combinations are possible: e.g., all files in a
   directory may export their modules via paths relative to a package name, and
   files elsewhere in the file system can import those modules by providing
   paths relative to that package name. So, e.g., /foo/bar.js may export its
   module via the name Foo/bar.js, with the name Foo may be specified in a
   config file under /foo, and other files may still be able to import it by
   that name when the /foo directory is moved to, say, /qux/foo. *)

(* hash table from module names to all known provider files.
   maintained and used by commit_modules and remove_files *)
let all_providers = Hashtbl.create 0

let add_provider f m =
  let provs = try SSet.add f (Hashtbl.find all_providers m)
    with Not_found -> SSet.singleton f in
  Hashtbl.replace all_providers m provs

let remove_provider f m =
  let provs = try SSet.remove f (Hashtbl.find all_providers m)
    with Not_found -> failwith (spf
      "can't remove provider %s of %S, not found in all_providers" f m)
  in
  Hashtbl.replace all_providers m provs

let get_providers = Hashtbl.find all_providers

(* inferred is a list of inferred file names.
   removed is a set of removed module names.
   modules provided by inferred files may or may not have a provider.
   modules named in removed definitely do not have a provider.

   Preconditions:
   1. all files in inferred have entries in InfoHeap (true if
   infer is properly calling add_module_info for every file
   successfully inferred)
   2. all modules not mentioned in removed, but provided by one or more
   files in InfoHeap, have some provider registered in NameHeap.
   (However, the current provider may not be the one we now want,
   given newly inferred files.)
   3. conversely all modules in removed lack a provider in NameHeap.

   Postconditions:
   1. all modules provided by at least 1 file
   in InfoHeap have a provider registered in NameHeap, and it's the
   provider we want according to our precedence and scoping rules.

   We make use of a shadow map in the master process which maintains
   a view of what's going on in NameHeap and InfoHeap, mapping module
   names to sets of filenames of providers.

   Algorithm here:
   1. add all removed modules to the set of modules to repick a provider for.
   2. add the modules provided by all inferred files to the repick set.
   3. for each module in the repick set, pick a winner from its available
   providers. if it's different than the current provider, or if there is no
   current provider, add the new provider to the list to be registered.
   4. remove the unregistered modules from NameHeap
   5. register the new providers in NameHeap
 *)
let commit_modules inferred removed =
  Modes.debug_string (fun () ->
    spf "*** committing modules inferred %d removed %d ***"
    (List.length inferred) (SSet.cardinal removed));
  (* all removed modules must be repicked *)
  (* all modules provided by newly inferred files must be repicked *)
  let repick = List.fold_left (fun acc f ->
    let { _module = m; _ } = get_module_info f in
    add_provider f m; add_provider f f;
    acc |> SSet.add m |> SSet.add f
  ) removed inferred in
  (* prep for registering new mappings in NameHeap *)
  let remove, replace, errs = SSet.fold (fun m (rem, rep, errs) ->
    match get_providers m with
    | ps when SSet.cardinal ps = 0 ->
        Modes.debug_string (fun () -> spf
          "no remaining providers: %S" m);
        (SSet.add m rem), rep, errs
    | ps ->
      let p, errs = choose_provider m ps errs in
      match NameHeap.get m with
      | Some f when f = p ->
          Modes.debug_string (fun () -> spf
            "unchanged provider: %S -> %s" m p);
          rem, rep, errs
      | Some f ->
          Modes.debug_string (fun () -> spf
            "new provider: %S -> %s replaces %s" m p f);
          (SSet.add m rem), ((m, p) :: rep), errs
      | None ->
          Modes.debug_string (fun () -> spf
            "initial provider %S -> %s" m p);
          (SSet.add m rem), ((m, p) :: rep), errs
  ) repick (SSet.empty, [], ([], [])) in
  (* update NameHeap *)
  NameHeap.remove_batch remove;
  SharedMem.collect ();
  List.iter (fun (m, p) -> NameHeap.add m p; NameHeap.add p p) replace;
  (* now that providers are updated, update reverse dependency info *)
  add_reverse_imports inferred;
  Modes.debug_string (fun () -> "*** done committing modules ***");
  errs

(* remove module mappings for given files, if they exist. Possibilities:
   1. file is current registered module provider for a given module name
   2. file is not current provider, but info is still registered
   3. file isn't in the map at all. This is an error.
   We return the set of module names whose current providers have been
   removed (#1). This is the set commit_module expects as its second
   argument.
*)
let remove_files files =
  (* files may or may not be registered as module providers.
     when they are, we need to clear their registrations *)
  let names = SSet.fold (fun file names ->
      match InfoHeap.get file with
      | Some info ->
          let { _module = name; required = requires; _ } = info in
          remove_provider file name;
          (match NameHeap.get name with
          | Some f when f = file -> (
              (* untrack all imports from this file. we only do this for
                 module providers to avoid inconsistencies when there are
                 multiple providers. *)
              reverse_imports_untrack name requires;
              (* when we delete a module and it was never referenced we need
                 to delete it from the reverse import heap. Otherwise, it can
                 still be looked up by using get-imported-by and that is
                 inconsistent. However, we only do so when we are the actual
                 module provider. This makes sure that exsiting modules won't
                 get erased when a duplicate is removed.
              *)
              match reverse_imports_get name with
              | Some reverse_imports ->
                  if SSet.is_empty reverse_imports
                  then reverse_imports_clear name
              | None -> ()
              );
              names |> SSet.add name |> SSet.add file
          | _ ->
              names
        )
      | None ->
          names
  ) files SSet.empty in
  (* clear any registrations that point to infos we're about to clear *)
  (* for infos, remove_batch will ignore missing entries, no need to filter *)
  NameHeap.remove_batch names;
  InfoHeap.remove_batch files;
  SharedMem.collect ();
  (* note: only return names of modules actually removed *)
  names

(*****************************************************************************)
(* The following code used to output the dependency graph in GraphML format. *)
(*****************************************************************************)

(*
  let oc = open_out "flow.graphml" in
  output_string oc "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns/graphml\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
         xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns/graphml
  http://www.yworks.com/xml/schema/graphml/1.0/ygraphml.xsd\"
         xmlns:y=\"http://www.yworks.com/xml/graphml\">
  <key id=\"D0\" for=\"node\" yfiles.type=\"nodegraphics\"/>
  <graph id=\"G0\" edgedefault=\"directed\">
";
  !dependencies |> SMap.iter (fun m (_,requires) ->
    output_string oc ("
<node id=\"" ^ m ^ "\">
      <data key=\"D0\">
        <y:ShapeNode>
          <y:NodeLabel>" ^ m ^ "</y:NodeLabel>
        </y:ShapeNode>
      </data>
    </node>
");
    requires |> SSet.iter (fun r ->
      if (SMap.mem r !dependencies) then
        output_string oc ("
<edge source=\"" ^ m ^ "\" target=\"" ^ r ^ "\"/>
")
      else ()
    )
  );
  output_string oc "
  </graph>
</graphml>
";
  close_out oc

*)
