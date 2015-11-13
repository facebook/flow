(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Coverage_level
open Utils
open Sys_utils

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)

type mode =
  | Ai of Ai_options.prepared
  | Autocomplete
  | Color
  | Coverage
  | DumpSymbolInfo
  | Errors
  | Lint
  | Suggest
  | NoBuiltins

type options = {
  filename : string;
  mode : mode;
}

let builtins_filename =
  Relative_path.create Relative_path.Dummy "builtins.hhi"

let what_builtins mode = match mode with
  NoBuiltins -> ""
  | _ -> "<?hh // decl\n"^
  "interface Traversable<+Tv> {}\n"^
  "interface Container<+Tv> extends Traversable<Tv> {}\n"^
  "interface Iterator<+Tv> extends Traversable<Tv> {}\n"^
  "interface Iterable<+Tv> extends Traversable<Tv> {}\n"^
  "interface KeyedTraversable<+Tk, +Tv> extends Traversable<Tv> {}\n"^
  "interface KeyedContainer<+Tk, +Tv> extends Container<Tv>, KeyedTraversable<Tk,Tv> {}\n"^
  "interface KeyedIterator<+Tk, +Tv> extends KeyedTraversable<Tk, Tv>, Iterator<Tv> {}\n"^
  "interface KeyedIterable<+Tk, +Tv> extends KeyedTraversable<Tk, Tv>, Iterable<Tv> {}\n"^
  "interface Awaitable<+T> {"^
  "  public function getWaitHandle(): WaitHandle<T>;"^
  "}\n"^
  "interface WaitHandle<+T> extends Awaitable<T> {}\n"^
  "interface ConstVector<+Tv> extends KeyedIterable<int, Tv>, KeyedContainer<int, Tv>{"^
  "  public function map<Tu>((function(Tv): Tu) $callback): ConstVector<Tu>;"^
  "}\n"^
  "interface ConstSet<+Tv> extends KeyedIterable<mixed, Tv>, Container<Tv>{}\n"^
  "interface ConstMap<+Tk, +Tv> extends KeyedIterable<Tk, Tv>, KeyedContainer<Tk, Tv>{"^
  "  public function map<Tu>((function(Tv): Tu) $callback): ConstMap<Tk, Tu>;"^
  "  public function mapWithKey<Tu>((function(Tk, Tv): Tu) $fn): ConstMap<Tk, Tu>;"^
  "}\n"^
  "final class Vector<Tv> implements ConstVector<Tv>{\n"^
  "  public function map<Tu>((function(Tv): Tu) $callback): Vector<Tu>;\n"^
  "  public function filter((function(Tv): bool) $callback): Vector<Tv>;\n"^
  "  public function reserve(int $sz): void;"^
  "  public function add(Tv $value): Vector<Tv>;"^
  "  public function addAll(?Traversable<Tv> $it): Vector<Tv>;"^
  "}\n"^
  "final class ImmVector<+Tv> implements ConstVector<Tv> {"^
  "  public function map<Tu>((function(Tv): Tu) $callback): ImmVector<Tu>;"^
  "}\n"^
  "final class Map<Tk, Tv> implements ConstMap<Tk, Tv> {"^
  "  /* HH_FIXME[3007]: This is intentional; not a constructor */"^
  "  public function map<Tu>((function(Tv): Tu) $callback): Map<Tk, Tu>;"^
  "  public function mapWithKey<Tu>((function(Tk, Tv): Tu) $fn): Map<Tk, Tu>;"^
  "  public function contains(Tk $k): bool;"^
  "}\n"^
  "final class ImmMap<+Tk, +Tv> implements ConstMap<Tk, Tv>{"^
  "  public function map<Tu>((function(Tv): Tu) $callback): ImmMap<Tk, Tu>;"^
  "  public function mapWithKey<Tu>((function(Tk, Tv): Tu) $fn): ImmMap<Tk, Tu>;"^
  "}\n"^
  "final class StableMap<Tk, Tv> implements ConstMap<Tk, Tv> {"^
  "  public function map<Tu>((function(Tv): Tu) $callback): StableMap<Tk, Tu>;"^
  "  public function mapWithKey<Tu>((function(Tk, Tv): Tu) $fn): StableMap<Tk, Tu>;"^
  "}\n"^
  "final class Set<Tv> implements ConstSet<Tv> {}\n"^
  "final class ImmSet<+Tv> implements ConstSet<Tv> {}\n"^
  "class Exception {"^
  "  public function __construct(string $x) {}"^
  "  public function getMessage(): string;"^
  "}\n"^
  "class Generator<+Tk, +Tv, -Ts> implements KeyedIterator<Tk, Tv> {\n"^
  "  public function next(): void;\n"^
  "  public function current(): Tv;\n"^
  "  public function key(): Tk;\n"^
  "  public function rewind(): void;\n"^
  "  public function valid(): bool;\n"^
  "  public function send(?Ts $v): void;\n"^
  "}\n"^
  "final class Pair<+Tk, +Tv> implements KeyedContainer<int,mixed> {public function isEmpty(): bool {}}\n"^
  "interface Stringish {public function __toString(): string {}}\n"^
  "interface XHPChild {}\n"^
  "function hh_show($val) {}\n"^
  "interface Countable { public function count(): int; }\n"^
  "interface AsyncIterator<+Tv> {}\n"^
  "interface AsyncKeyedIterator<+Tk, +Tv> extends AsyncIterator<Tv> {}\n"^
  "class AsyncGenerator<+Tk, +Tv, -Ts> implements AsyncKeyedIterator<Tk, Tv> {\n"^
  "  public function next(): Awaitable<?(Tk, Tv)> {}\n"^
  "  public function send(?Ts $v): Awaitable<?(Tk, Tv)> {}\n"^
  "  public function raise(Exception $e): Awaitable<?(Tk, Tv)> {}"^
  "}\n"^
  "function isset($x): bool;"^
  "function empty($x): bool;"^
  "function unset($x): void;"^
  "namespace HH {\n"^
  "abstract class BuiltinEnum<T> {\n"^
  "  final public static function getValues(): array<string, T>;\n"^
  "  final public static function getNames(): array<T, string>;\n"^
  "  final public static function coerce(mixed $value): ?T;\n"^
  "  final public static function assert(mixed $value): T;\n"^
  "  final public static function isValid(mixed $value): bool;\n"^
  "  final public static function assertAll(Traversable<mixed> $values): Container<T>;\n"^
  "}\n"^
  "}\n"^
  "function array_map($x, $y, ...);\n"^
  "function idx<Tk, Tv>(?KeyedContainer<Tk, Tv> $c, $i, $d = null) {}\n"^
  "final class stdClass {}\n" ^
  "function rand($x, $y): int;\n" ^
  "function invariant($x, ...): void;\n" ^
  "function exit(int $exit_code_or_message = 0): noreturn;\n" ^
  "function invariant_violation(...): noreturn;\n" ^
  "function get_called_class(): string;\n" ^
  "abstract final class Shapes {\n" ^
  "  public static function idx(shape() $shape, arraykey $index, $default = null) {}\n" ^
  "  public static function keyExists(shape() $shape, arraykey $index): bool {}\n" ^
  "  public static function removeKey(shape() $shape, arraykey $index): void {}\n" ^
  "}\n" ^
  "newtype typename<+T> as string = string;\n"^
  "newtype classname<+T> as typename<T> = typename<T>;\n" ^
 "function var_dump($x): void;\n" ^
  "function gena();\n" ^
  "function genva();\n" ^
  "function gen_array_rec();\n"^
  "function is_int(mixed $x): bool {}\n"^
  "function is_bool(mixed $x): bool {}\n"^
  "function is_float(mixed $x): bool {}\n"^
  "function is_string(mixed $x): bool {}\n"^
  "function is_null(mixed $x): bool {}\n"^
  "function is_array(mixed $x): bool {}\n"^
  "function is_resource(mixed $x): bool {}\n"^
  "interface IMemoizeParam {\n"^
  "  public function getInstanceKey(): string;\n"^
  "}\n"^
  "newtype TypeStructure<T> as shape(\n"^
  "  'kind'=> int,\n"^
  "  'nullable'=>?bool,\n"^
  "  'classname'=>?classname<T>,\n"^
  "  'elem_types' => ?array,\n"^
  "  'param_types' => ?array,\n"^
  "  'return_type' => ?array,\n"^
  "  'generic_types' => ?array,\n"^
  "  'fields' => ?array,\n"^
  "  'name' => ?string,\n"^
  "  'alias' => ?string,\n"^
  ") = shape(\n"^
  "  'kind'=> int,\n"^
  "  'nullable'=>?bool,\n"^
  "  'classname'=>?classname<T>,\n"^
  "  'elem_types' => ?array,\n"^
  "  'param_types' => ?array,\n"^
  "  'return_type' => ?array,\n"^
  "  'generic_types' => ?array,\n"^
  "  'fields' => ?array,\n"^
  "  'name' => ?string,\n"^
  "  'alias' => ?string,\n"^
  ");\n"^
  "function type_structure($x, $y);\n"^
  "const int __LINE__ = 0;\n"^
  "const string __CLASS__ = '';\n"^
  "const string __TRAIT__ = '';\n"^
  "const string __FILE__ = '';\n"^
  "const string __DIR__ = '';\n"^
  "const string __FUNCTION__ = '';\n"^
  "const string __METHOD__ = '';\n"^
  "const string __NAMESPACE__ = '';\n"^
  "interface Indexish<+Tk, +Tv> extends KeyedContainer<Tk, Tv> {}\n"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let die str =
  let oc = stderr in
  output_string oc str;
  close_out oc;
  exit 2

let error l = output_string stderr (Errors.to_string (Errors.to_absolute l))

let parse_options () =
  let fn_ref = ref None in
  let usage = Printf.sprintf "Usage: %s filename\n" Sys.argv.(0) in
  let mode = ref Errors in
  let set_mode x () =
    if !mode <> Errors
    then raise (Arg.Bad "only a single mode should be specified")
    else mode := x in
  let set_ai x = set_mode (Ai (Ai_options.prepare ~server:false x)) () in
  let options = [
    "--ai",
      Arg.String (set_ai),
      "Run the abstract interpreter";
    "--auto-complete",
      Arg.Unit (set_mode Autocomplete),
      "Produce autocomplete suggestions";
    "--colour",
      Arg.Unit (set_mode Color),
      "Produce colour output";
    "--color",
      Arg.Unit (set_mode Color),
      "Produce color output";
    "--coverage",
      Arg.Unit (set_mode Coverage),
      "Produce coverage output";
    "--dump-symbol-info",
      Arg.Unit (set_mode DumpSymbolInfo),
      "Dump all symbol information";
    "--lint",
      Arg.Unit (set_mode Lint),
      "Produce lint errors";
    "--suggest",
      Arg.Unit (set_mode Suggest),
      "Suggest missing typehints";
    "--no-builtins",
      Arg.Unit (set_mode NoBuiltins),
      "Don't use builtins (e.g. ConstSet)";
  ] in
  Arg.parse options (fun fn -> fn_ref := Some fn) usage;
  let fn = match !fn_ref with
    | Some fn -> fn
    | None -> die usage in
  { filename = fn;
    mode = !mode;
  }

let suggest_and_print nenv fn { FileInfo.funs; classes; typedefs; consts; _ } =
  let make_set =
    List.fold_left ~f: (fun acc (_, x) -> SSet.add x acc) ~init: SSet.empty in
  let n_funs = make_set funs in
  let n_classes = make_set classes in
  let n_types = make_set typedefs in
  let n_consts = make_set consts in
  let names = { FileInfo.n_funs; n_classes; n_types; n_consts } in
  let fast = Relative_path.Map.add fn names Relative_path.Map.empty in
  let patch_map = Typing_suggest_service.go None nenv fast in
  match Relative_path.Map.get fn patch_map with
    | None -> ()
    | Some l -> begin
      (* Sort so that the unit tests come out in a consistent order, normally
       * doesn't matter. *)
      let l = List.sort ~cmp: (fun (x, _, _) (y, _, _) -> x - y) l in
      List.iter ~f: (ServerConvert.print_patch fn) l
    end

(* This allows to fake having multiple files in one file. This
 * is used only in unit test files.
 * Indeed, there are some features that require mutliple files to be tested.
 * For example, newtype has a different meaning depending on the file.
 *)
let rec make_files = function
  | [] -> []
  | Str.Delim header :: Str.Text content :: rl ->
      let pattern = Str.regexp "////" in
      let header = Str.global_replace pattern "" header in
      let pattern = Str.regexp "[ ]*" in
      let filename = Str.global_replace pattern "" header in
      (filename, content) :: make_files rl
  | _ -> assert false

(* We have some hacky "syntax extensions" to have one file contain multiple
 * files, which can be located at arbitrary paths. This is useful e.g. for
 * testing lint rules, some of which activate only on certain paths. It's also
 * useful for testing abstract types, since the abstraction is enforced at the
 * file boundary.
 * Takes the path to a single file, returns a map of filenames to file contents.
 *)
let file_to_files file =
  let abs_fn = Relative_path.to_absolute file in
  let content = cat abs_fn in
  let delim = Str.regexp "////.*" in
  if Str.string_match delim content 0
  then
    let contentl = Str.full_split delim content in
    let files = make_files contentl in
    List.fold_left ~f: begin fun acc (sub_fn, content) ->
      let file =
        Relative_path.create Relative_path.Dummy (abs_fn^"--"^sub_fn) in
      Relative_path.Map.add file content acc
    end ~init: Relative_path.Map.empty files
  else if str_starts_with content "// @directory " then
    let contentl = Str.split (Str.regexp "\n") content in
    let first_line = List.hd_exn contentl in
    let regexp = Str.regexp "^// @directory *\\([^ ]*\\)" in
    let has_match = Str.string_match regexp first_line 0 in
    assert has_match;
    let dir = Str.matched_group 1 first_line in
    let file = Relative_path.create Relative_path.Dummy (dir ^ abs_fn) in
    let content = String.concat "\n" (List.tl_exn contentl) in
    Relative_path.Map.singleton file content
  else
    Relative_path.Map.singleton file content

(* Make readable test output *)
let replace_color input =
  match input with
  | (Some Unchecked, str) -> "<unchecked>"^str^"</unchecked>"
  | (Some Checked, str) -> "<checked>"^str^"</checked>"
  | (Some Partial, str) -> "<partial>"^str^"</partial>"
  | (None, str) -> str

let print_colored fn type_acc =
  let content = cat (Relative_path.to_absolute fn) in
  let results = ColorFile.go content type_acc in
  if Unix.isatty Unix.stdout
  then Tty.print (ClientColorFile.replace_colors results)
  else print_string (List.map ~f: replace_color results |> String.concat "")

let print_coverage fn type_acc =
  let counts = ServerCoverageMetric.count_exprs fn type_acc in
  ClientCoverageMetric.go ~json:false (Some (Leaf counts))

let handle_mode mode filename nenv files_contents files_info errors ai_results =
  match mode with
  | Ai _ -> ()
  | Autocomplete ->
      let file = cat (Relative_path.to_absolute filename) in
      let result = ServerAutoComplete.auto_complete nenv file in
      List.iter ~f: begin fun r ->
        let open AutocompleteService in
        Printf.printf "%s %s\n" r.res_name r.res_ty
      end result
  | Color ->
      Relative_path.Map.iter begin fun fn fileinfo ->
        if fn = builtins_filename then () else begin
          let result = ServerColorFile.get_level_list begin fun () ->
            ignore @@ Typing_check_utils.check_defs nenv fileinfo;
            fn
          end in
          print_colored fn result;
        end
      end files_info
  | Coverage ->
      Relative_path.Map.iter begin fun fn fileinfo ->
        if fn = builtins_filename then () else begin
          let type_acc = ServerCoverageMetric.accumulate_types fileinfo in
          print_coverage fn type_acc;
        end
      end files_info
  | DumpSymbolInfo ->
      begin match Relative_path.Map.get filename files_info with
        | Some fileinfo ->
            let raw_result =
              SymbolInfoService.helper [] [(filename, fileinfo)] in
            let result = SymbolInfoService.format_result raw_result in
            let result_json = ClientSymbolInfo.to_json result in
            print_endline (Hh_json.json_to_string result_json)
        | None -> ()
      end
  | Lint ->
      let lint_errors =
        Relative_path.Map.fold begin fun fn content lint_errors ->
          lint_errors @ fst (Lint.do_ begin fun () ->
            Linting_service.lint fn content
          end)
        end files_contents [] in
      if lint_errors <> []
      then begin
        let lint_errors = List.sort ~cmp: begin fun x y ->
          Pos.compare (Lint.get_pos x) (Lint.get_pos y)
        end lint_errors in
        let lint_errors = List.map ~f: Lint.to_absolute lint_errors in
        ServerLint.output_text stdout lint_errors;
        exit 2
      end
      else Printf.printf "No lint errors\n"
  | Suggest
  | NoBuiltins
  | Errors ->
      let errors = Relative_path.Map.fold begin fun _ fileinfo errors ->
        errors @ Typing_check_utils.check_defs nenv fileinfo
      end files_info errors in
      if mode = Suggest
      then Relative_path.Map.iter (suggest_and_print nenv) files_info;
      if errors <> []
      then (error (List.hd_exn errors); exit 2)
      else Printf.printf "No errors\n"

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main_hack { filename; mode; } =
  if not Sys.win32 then
    ignore (Sys.signal Sys.sigusr1
              (Sys.Signal_handle Typing.debug_print_last_pos));
  EventLogger.init (Daemon.devnull ()) 0.0;
  let _handle = SharedMem.init_default () in
  let tmp_hhi = Path.concat Path.temp_dir_name "hhi" in
  Hhi.set_hhi_root_for_unit_test tmp_hhi;
  let outer_do f = match mode with
    | Ai ai_options ->
        let ai_results, inner_results =
          Ai.do_ Typing_check_utils.check_defs filename ai_options in
        ai_results, inner_results
    | _ ->
        let inner_results = f () in
        [], inner_results
  in
  let builtins = what_builtins mode in
  let filename = Relative_path.create Relative_path.Dummy filename in
  let files_contents = file_to_files filename in
  let ai_results, (errors, (nenv, files_info)) =
    outer_do begin fun () ->
      Errors.do_ begin fun () ->
        let parsed_files =
          Relative_path.Map.mapi Parser_hack.program files_contents in
        let parsed_builtins = Parser_hack.program builtins_filename builtins in
        let parsed_files =
          Relative_path.Map.add builtins_filename parsed_builtins parsed_files
        in

        let files_info =
          Relative_path.Map.mapi begin fun fn parsed_file ->
            let {Parser_hack.file_mode; comments; ast} = parsed_file in
            Parser_heap.ParserHeap.add fn ast;
            let funs, classes, typedefs, consts = Ast_utils.get_defs ast in
            { FileInfo.
              file_mode; funs; classes; typedefs; consts; comments;
              consider_names_just_for_autoload = false }
          end parsed_files in

        (* Note that nenv.Naming.itcopt remains TypecheckerOptions.default *)
        let nenv = Relative_path.Map.fold begin fun fn fileinfo nenv ->
          let {FileInfo.funs; classes; typedefs; consts; _} = fileinfo in
          Naming.make_env nenv ~funs ~classes ~typedefs ~consts
        end files_info (Naming.empty TypecheckerOptions.default) in

        let all_classes =
          Relative_path.Map.fold begin fun fn {FileInfo.classes; _} acc ->
            List.fold_left ~f: begin fun acc (_, cname) ->
              SMap.add cname (Relative_path.Set.singleton fn) acc
            end ~init: acc classes
          end files_info SMap.empty in

        Relative_path.Map.iter begin fun fn _ ->
          Typing_decl.make_env nenv all_classes fn
        end files_info;

        nenv, files_info
      end
    end in
  handle_mode mode filename nenv files_contents files_info errors ai_results

(* command line driver *)
let _ =
  if ! Sys.interactive
  then ()
  else
    (* On windows, setting 'binary mode' avoids to output CRLF on
       stdout.  The 'text mode' would not hurt the user in general, but
       it breaks the testsuite where the output is compared to the
       expected one (i.e. in given file without CRLF). *)
    set_binary_mode_out stdout true;
    let options = parse_options () in
    Unix.handle_unix_error main_hack options
