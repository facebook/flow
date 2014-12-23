(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Coverage_level
open Utils

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)

type options = {
  filename : string;
  suggest : bool;
  color : bool;
  coverage : bool;
  prolog : bool;
  rest : string list
}

let builtins_filename = "builtins.hhi"
let builtins = "<?hh // decl\n"^
  "interface Traversable<Tv> {}\n"^
  "interface Container<Tv> extends Traversable<Tv> {}\n"^
  "interface Iterator<Tv> extends Traversable<Tv> {}\n"^
  "interface Iterable<Tv> extends Traversable<Tv> {}\n"^
  "interface KeyedTraversable<Tk, Tv> extends Traversable<Tv> {}\n"^
  "interface KeyedContainer<Tk, Tv> extends Container<Tv>, KeyedTraversable<Tk,Tv> {}\n"^
  "interface Indexish<Tk, Tv> extends KeyedContainer<Tk, Tv> {}\n"^
  "interface KeyedIterator<Tk, Tv> extends KeyedTraversable<Tk, Tv>, Iterator<Tv> {}\n"^
  "interface KeyedIterable<Tk, Tv> extends KeyedTraversable<Tk, Tv>, Iterable<Tv> {}\n"^
  "interface Awaitable<T> {"^
  "  public function getWaitHandle(): WaitHandle<T>;"^
  "}\n"^
  "interface WaitHandle<T> extends Awaitable<T> {}\n"^
  "interface ConstVector<Tv> extends KeyedIterable<int, Tv>, Indexish<int, Tv>{"^
  "  public function map<Tu>((function(Tv): Tu) $callback): ConstVector<Tu>;"^
  "}\n"^
  "interface ConstSet<Tv> extends KeyedIterable<mixed, Tv>, Container<Tv>{}\n"^
  "interface ConstMap<Tk, Tv> extends KeyedIterable<Tk, Tv>, Indexish<Tk, Tv>{"^
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
  "final class ImmVector<Tv> implements ConstVector<Tv> {}\n"^
  "final class Map<Tk, Tv> implements ConstMap<Tk, Tv> {"^
  "  public function map<Tu>((function(Tv): Tu) $callback): Map<Tk, Tu>;"^
  "  public function contains(Tk $k): bool;"^
  "}\n"^
  "final class ImmMap<Tk, Tv> implements ConstMap<Tk, Tv>{}\n"^
  "final class StableMap<Tk, Tv> implements ConstMap<Tk, Tv> {}\n"^
  "final class Set<Tv> extends ConstSet<Tv> {}\n"^
  "final class ImmSet<Tv> extends ConstSet<Tv> {}\n"^
  "class Exception { public function __construct(string $x) {} }\n"^
  "class Generator<Tk, Tv, Ts> implements KeyedIterator<Tk, Tv> {\n"^
  "  public function next(): void;\n"^
  "  public function current(): Tv;\n"^
  "  public function key(): Tk;\n"^
  "  public function rewind(): void;\n"^
  "  public function valid(): bool;\n"^
  "  public function send(?Ts $v): void;\n"^
  "}\n"^
  "final class Pair<Tk, Tv> extends Indexish<int,mixed> {public function isEmpty(): bool {}}\n"^
  "interface Stringish {public function __toString(): string {}}\n"^
  "interface XHPChild {}\n"^
  "function hh_show($val) {}\n"^
  "interface Countable { public function count(): int; }\n"^
  "interface AsyncIterator<Tv> {}\n"^
  "interface AsyncKeyedIterator<Tk, Tv> extends AsyncIterator<Tv> {}\n"^
  "class AsyncGenerator<Tk, Tv, Ts> implements AsyncKeyedIterator<Tk, Tv> {\n"^
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
  "function idx<Tk, Tv>(?Indexish<Tk, Tv> $c, $i, $d = null) {}\n"^
  "final class stdClass {}\n"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let die str =
  let oc = stderr in
  output_string oc str;
  close_out oc;
  exit 2

let error l = die (Errors.to_string (Errors.to_absolute l))

let parse_options () =
  let fn_ref = ref None in
  let suggest = ref false in
  let color = ref false in
  let coverage = ref false in
  let prolog = ref false in
  let rest_options = ref [] in
  let rest x = rest_options := x :: !rest_options in
  let usage = Printf.sprintf "Usage: %s filename\n" Sys.argv.(0) in
  let options = [
    "--suggest",
      Arg.Set suggest,
      "Suggest missing typehints";
    "--color",
      Arg.Set color,
      "Produce color output";
    "--coverage",
      Arg.Set coverage,
      "Produce coverage output";
    "--prolog",
      Arg.Set prolog,
      "Produce prolog facts";
    "--",
      Arg.Rest rest,
      "";
  ] in
  Arg.parse options (fun fn -> fn_ref := Some fn) usage;
  let fn = match !fn_ref with
    | Some fn -> fn
    | None -> die usage in
  { filename = fn;
    suggest = !suggest;
    color = !color;
    coverage = !coverage;
    prolog = !prolog;
    rest = !rest_options;
  }

let suggest_and_print fn funs classes typedefs consts =
  let make_set =
    List.fold_left (fun acc (_, x) -> SSet.add x acc) SSet.empty in
  let n_funs = make_set funs in
  let n_classes = make_set classes in
  let n_types = make_set typedefs in
  let n_consts = make_set consts in
  let names = { FileInfo.n_funs; n_classes; n_types; n_consts } in
  let fast = Relative_path.Map.add fn names Relative_path.Map.empty in
  let patch_map = Typing_suggest_service.go None fast in
  match Relative_path.Map.get fn patch_map with
    | None -> ()
    | Some l -> begin
      (* Sort so that the unit tests come out in a consistent order, normally
       * doesn't matter. *)
      let l = List.sort (fun (x, _, _) (y, _, _) -> x - y) l in
      List.iter (ServerConvert.print_patch fn) l
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

let parse_file file =
  let abs_fn = Relative_path.to_absolute file in
  let content = cat abs_fn in
  let delim = Str.regexp "////.*" in
  if Str.string_match delim content 0
  then
    let contentl = Str.full_split delim content in
    let files = make_files contentl in
    List.fold_right begin fun (sub_fn, content) ast ->
      let file =
        Relative_path.create Relative_path.Dummy (abs_fn^"--"^sub_fn) in
      let {Parser_hack.is_hh_file; comments; ast = ast'} =
        Parser_hack.program file content
      in
      ast' @ ast
    end files []
  else begin
    let {Parser_hack.is_hh_file; comments; ast} =
      Parser_hack.program file content
    in
    ast
  end

(* collect definition names from parsed ast *)
let collect_defs ast =
  List.fold_right begin fun def (funs, classes, typedefs, consts) ->
    match def with
    | Ast.Fun f -> f.Ast.f_name :: funs, classes, typedefs, consts
    | Ast.Class c -> funs, c.Ast.c_name :: classes, typedefs, consts
    | Ast.Typedef td -> funs, classes, td.Ast.t_id :: typedefs, consts
    | Ast.Constant cst -> funs, classes, typedefs, cst.Ast.cst_name :: consts
    | _ -> funs, classes, typedefs, consts
  end ast ([], [], [], [])

(* Make readable test output *)
let replace_color input =
  match input with
  | (Some Unchecked, str) -> "<unchecked>"^str^"</unchecked>"
  | (Some Checked, str) -> "<checked>"^str^"</checked>"
  | (Some Partial, str) -> "<partial>"^str^"</partial>"
  | (None, str) -> str

let print_colored fn =
  let content = cat (Relative_path.to_absolute fn) in
  let pos_level_l = mk_level_list (Some fn) !Typing_defs.type_acc in
  let raw_level_l =
    rev_rev_map (fun (p, cl) -> Pos.info_raw p, cl) pos_level_l in
  let results = ColorFile.go content raw_level_l in
  if Unix.isatty Unix.stdout
  then Tty.print (ClientColorFile.replace_colors results)
  else print_string (List.map replace_color results |> String.concat "")

let print_coverage fn =
  let counts = ServerCoverageMetric.count_exprs fn !Typing_defs.type_acc in
  ClientCoverageMetric.go false (Some (Leaf counts))

let print_prolog funs classes typedefs consts =
  let facts = Prolog.facts_of_defs [] funs classes typedefs consts in
  PrologMain.output_facts stdout facts

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* This was the original main ... before there was a daemon.
 * This function can also be called interactively from top_single to
 * populate the global typing environment (see typing_env.ml) for
 * a given file. You can then inspect this typing environment, e.g.
 * with 'Typing_env.Classes.get "Foo";;'
 *)
let main_hack { filename; suggest; color; coverage; prolog; _ } =
  ignore (Sys.signal Sys.sigusr1 (Sys.Signal_handle Typing.debug_print_last_pos));
  SharedMem.init();
  Hhi.set_hhi_root_for_unit_test (Path.mk_path "/tmp/hhi");
  let errors, () =
    Errors.do_ begin fun () ->
      let file = Relative_path.create Relative_path.Dummy builtins_filename in
      let {Parser_hack.is_hh_file; comments; ast = ast_builtins} =
        Parser_hack.program file builtins
      in
      let filename = Relative_path.create Relative_path.Dummy filename in
      let ast_file = parse_file filename in
      let ast = ast_builtins @ ast_file in
      Parser_heap.ParserHeap.add filename ast;
      let funs, classes, typedefs, consts = collect_defs ast in
      let nenv = Naming.make_env Naming.empty ~funs ~classes ~typedefs ~consts in
      let all_classes = List.fold_right begin fun (_, cname) acc ->
        SMap.add cname (Relative_path.Set.singleton filename) acc
      end classes SMap.empty in
      Typing_decl.make_env nenv all_classes filename;
      Typing_defs.accumulate_types := color || coverage;
      List.iter (fun (_, fname) -> Typing_check_service.type_fun fname) funs;
      List.iter (fun (_, cname) -> Typing_check_service.type_class cname) classes;
      List.iter (fun (_, x) -> Typing_check_service.check_typedef x) typedefs;
      if prolog
      then print_prolog funs classes typedefs consts;
      if color
      then print_colored filename;
      if coverage
      then print_coverage filename;
      if suggest
      then suggest_and_print filename funs classes typedefs consts
    end
  in
  if not prolog then begin
    if errors <> []
    then error (List.hd errors)
    else Printf.printf "No errors\n"
  end

(* command line driver *)
let _ =
  if ! Sys.interactive
  then ()
  else
    let options = parse_options () in
    main_hack options
