(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
open Lexer_hack
open Ast

module L = Lexer_hack

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

type env = {
  file      : Relative_path.t;
  mode      : FileInfo.mode;
  priority  : int;
  lb        : Lexing.lexbuf;
  errors    : (Pos.t * string) list ref;
  in_generator : bool ref;
}

let init_env file lb = {
  file     = file;
  mode     = FileInfo.Mpartial;
  priority = 0;
  lb       = lb;
  errors   = ref [];
  in_generator = ref false;
}

type parser_return = {
  file_mode  : FileInfo.mode option; (* None if PHP *)
  comments   : (Pos.t * string) list;
  ast        : Ast.program;
}

(*****************************************************************************)
(* Lexer (with backtracking) *)
(*****************************************************************************)

type saved_lb = {
  (* no need to save refill_buff because it's constant *)
  lex_abs_pos     : int;
  lex_start_pos   : int;
  lex_curr_pos    : int;
  lex_last_pos    : int;
  lex_last_action : int;
  lex_eof_reached : bool;
  lex_mem         : int array;
  lex_start_p     : Lexing.position;
  lex_curr_p      : Lexing.position;
}

let save_lexbuf_state (lb : Lexing.lexbuf) : saved_lb =
  {
    lex_abs_pos     = lb.Lexing.lex_abs_pos;
    lex_start_pos   = lb.Lexing.lex_start_pos;
    lex_curr_pos    = lb.Lexing.lex_curr_pos;
    lex_last_pos    = lb.Lexing.lex_last_pos;
    lex_last_action = lb.Lexing.lex_last_action;
    lex_eof_reached = lb.Lexing.lex_eof_reached;
    lex_mem         = lb.Lexing.lex_mem;
    lex_start_p     = lb.Lexing.lex_start_p;
    lex_curr_p      = lb.Lexing.lex_curr_p;
  }

let restore_lexbuf_state (lb : Lexing.lexbuf) (saved : saved_lb) : unit =
  begin
    lb.Lexing.lex_abs_pos     <- saved.lex_abs_pos;
    lb.Lexing.lex_start_pos   <- saved.lex_start_pos;
    lb.Lexing.lex_curr_pos    <- saved.lex_curr_pos;
    lb.Lexing.lex_last_pos    <- saved.lex_last_pos;
    lb.Lexing.lex_last_action <- saved.lex_last_action;
    lb.Lexing.lex_eof_reached <- saved.lex_eof_reached;
    lb.Lexing.lex_mem         <- saved.lex_mem;
    lb.Lexing.lex_start_p     <- saved.lex_start_p;
    lb.Lexing.lex_curr_p      <- saved.lex_curr_p;
  end

(*
 * Call a function with a forked lexing environment, and return its
 * result.
 *)
let look_ahead (env : env) (f : env -> 'a) : 'a =
  let saved = save_lexbuf_state env.lb in
  let ret = f env in
  restore_lexbuf_state env.lb saved;
  ret

(*
 * Conditionally parse, saving lexer state in case we need to backtrack.
 * The function parameter returns any optional type.  If it's None, pop
 * lexer state on the way out.
 *
 * Note that you shouldn't add any errors to the environment before
 * you've committed to returning Some something.  The error state is not
 * popped.
 *)
let try_parse (env : env) (f : env -> 'a option) : 'a option =
  let saved = save_lexbuf_state env.lb in
  match f env with
  | Some x -> Some x
  | None   -> (restore_lexbuf_state env.lb saved; None)

(* Return the next token without updating lexer state *)
let peek env =
  let saved = save_lexbuf_state env.lb in
  let ret = L.token env.file env.lb in
  restore_lexbuf_state env.lb saved;
  ret

(* Checks if the next token matches a given word without updating lexer state*)
let peek_check_word env word =
  let saved = save_lexbuf_state env.lb in
  let ret = L.token env.file env.lb = Tword && Lexing.lexeme env.lb = word in
  restore_lexbuf_state env.lb saved;
  ret

(* Drop the next token unconditionally *)
let drop (env : env) : unit = match L.token env.file env.lb with _ -> ()

let btw (p1, _) (p2, _) = Pos.btw p1 p2

(*****************************************************************************)
(* Errors *)
(*****************************************************************************)

let error_at env pos msg =
  env.errors := (pos, msg) :: !(env.errors)

let error env msg =
  error_at env (Pos.make env.file env.lb) msg

let error_continue env =
  error env
    "Yeah...we're not going to support continue/break N. \
    It makes static analysis tricky and it's not really essential"

let error_back env msg =
  let pos = Pos.make env.file env.lb in
  L.back env.lb;
  error_at env pos msg

let error_expect env expect =
  error_back env ("Expected "^expect)

let expect env x =
  if L.token env.file env.lb = x
  then ()
  else error_expect env (L.token_to_string x)

let expect_word env name =
  let tok = L.token env.file env.lb in
  let value = Lexing.lexeme env.lb in
  if tok <> Tword || value <> name
  then error_expect env ("'"^name^ "' (not '"^value^"')");
  ()

(*****************************************************************************)
(* Modifiers checks (public private, final abstract etc ...)  *)
(*****************************************************************************)

let rec check_modifiers env pos abstract final = function
  | [] -> ()
  | Final :: _ when abstract ->
      error_at env pos "Parse error. Cannot mix final and abstract"
  | Abstract :: _ when final ->
      error_at env pos "Parse error. Cannot mix final and abstract"
  | Final :: rl -> check_modifiers env pos abstract true rl
  | Abstract :: rl -> check_modifiers env pos true final rl
  | _ :: rl -> check_modifiers env pos abstract final rl

let check_visibility env pos l =
  if List.exists begin function
    | Private | Public | Protected | Static -> true
    | _ -> false
  end l
  then ()
  else error_at env pos
      "Parse error. You are missing public, private or protected."

let rec check_mix_visibility env pos last_vis = function
  | [] -> ()
  | (Private | Public | Protected as vis) :: rl ->
      (match last_vis with
      | Some vis2 when vis <> vis2 ->
          error_at env pos
            "Parse error. Cannot mix different visibilities."
      | _ ->
          check_mix_visibility env pos (Some vis) rl
      )
  | _ :: rl -> check_mix_visibility env pos last_vis rl

let rec check_duplicates env pos = function
  | [_] | [] -> ()
  | Private :: rl -> check_duplicates env pos rl
  | x :: (y :: _) when x = y ->
      error_at env pos "Parse error. Duplicate modifier"
  | _ :: rl -> check_duplicates env pos rl

let check_modifiers env pos l =
  check_visibility env pos l;
  check_modifiers env pos false false l;
  check_duplicates env pos (List.sort compare l);
  check_mix_visibility env pos None l;
  ()

let check_not_final env pos modifiers =
  if List.exists (function Final -> true | _ -> false) modifiers
  then error_at env pos "class variable cannot be final";
  ()

let check_toplevel env pos =
  if env.mode = FileInfo.Mstrict
  then error_at env pos "Remove all toplevel statements except for requires"

(*****************************************************************************)
(* Check expressions. *)
(*****************************************************************************)

let rec check_lvalue env = function
  | pos, Obj_get (_, (_, Id (_, name)), OG_nullsafe) ->
      error_at env pos "?-> syntax is not supported for lvalues"
  | pos, Obj_get (_, (_, Id (_, name)), _) when name.[0] = ':' ->
      error_at env pos "->: syntax is not supported for lvalues"
  | _, (Lvar _ | Obj_get _ | Array_get _ | Class_get _ | Unsafeexpr _) -> ()
  | pos, Call ((_, Id (_, "tuple")), _, _) ->
      error_at env pos
        "Tuple cannot be used as an lvalue. Maybe you meant List?"
  | _, List el -> List.iter (check_lvalue env) el
  | pos, (Array _ | Shape _ | Collection _
  | Null | True | False | Id _ | Clone _
  | Class_const _ | Call _ | Int _ | Float _
  | String _ | String2 _ | Yield _ | Yield_break
  | Await _ | Expr_list _ | Cast _ | Unop _
  | Binop _ | Eif _ | InstanceOf _ | New _ | Efun _ | Lfun _ | Xml _
  | Import _ | Ref _) ->
      error_at env pos "Invalid lvalue"

(* The bound variable of a foreach can be a reference (but not inside
  a list expression. *)
let check_foreach_lvalue env = function
  | (_, Ref e) | e -> check_lvalue env e

(*****************************************************************************)
(* Operator priorities.
 *
 * It is annoying to deal with priorities by hand (although it's possible).
 * This list mimics what would typically look like yacc rules, defining
 * the operators priorities (from low to high), and associativity (left, right
 * or non-assoc).
 *
 * The priorities are then used by the "reducer" to auto-magically parse
 * expressions in the right order (left, right, non-assoc) and with the right
 * priority. Checkout the function "reduce" for more details.
 *)
(*****************************************************************************)

type assoc =
  | Left       (* a <op> b <op> c = ((a <op> b) <op> c) *)
  | Right      (* a <op> b <op> c = (a <op> (b <op> c)) *)
  | NonAssoc   (* a <op> b <op> c = error *)

let priorities = [
  (* Lowest priority *)
  (NonAssoc, [Tyield]);
  (NonAssoc, [Tawait]);
  (Left, [Timport; Teval;]);
  (Left, [Tcomma]);
  (Right, [Tprint]);
  (Left, [Tqm; Tcolon]);
  (Left, [Tbarbar]);
  (Left, [Txor]);
  (Left, [Tampamp]);
  (Left, [Tbar]);
  (Left, [Tamp]);
  (NonAssoc, [Teqeq; Tdiff; Teqeqeq; Tdiff2]);
  (NonAssoc, [Tlt; Tlte; Tgt; Tgte]);
  (Left, [Tltlt; Tgtgt]);
  (Left, [Tplus; Tminus; Tdot]);
  (Left, [Tstar; Tslash; Tpercent]);
  (Right, [Tem]);
  (NonAssoc, [Tinstanceof]);
  (Right, [Ttild; Tincr; Tdecr; Tcast]);
  (Right, [Tstarstar]);
  (Right, [Tat; Tref]);
  (Left, [Tlp]);
  (NonAssoc, [Tnew; Tclone]);
  (Left, [Tlb]);
  (Right, [Teq; Tpluseq; Tminuseq; Tstareq;
           Tslasheq; Tdoteq; Tpercenteq;
           Tampeq; Tbareq; Txoreq; Tlshifteq; Trshifteq]);
  (Left, [Tarrow; Tnsarrow]);
  (Left, [Telseif]);
  (Left, [Telse]);
  (Left, [Tendif]);
  (Left, [Tcolcol]);
  (Left, [Tdollar]);
  (* Highest priority *)
]

let get_priority =
  (* Creating the table of assocs/priorities at initialization time. *)
  let ptable = Hashtbl.create 23 in
  (* Lowest priority = 0 *)
  let priority = ref 0 in
  List.iter begin fun (assoc, tokl) ->
    List.iter begin fun token ->
      (* Associates operator => (associativity, priority) *)
      Hashtbl.add ptable token (assoc, !priority)
    end tokl;
    (* This is a bit subtle:
     *
     * The difference in priority between 2 lines should be 2, not 1.
     *
     * It's because of a trick we use in the reducer.
     * For something to be left-associative, we just pretend
     * that the right hand side expression has a higher priority.
     *
     * An example:
     * expr "1 + 2 + 3"
     * reduce (e1 = 1) "2 + 3"  // priority = 0
     * reduce (e1 = 1) (expr "2 + 3" with priority+1)
     * reduce (e1 = 1) (2, "+ 3") <--- this is where the trick is:
     *                                 because we made the priority higher
     *                                 the reducer stops when it sees the
     *                                 "+" sign.
     *)
    priority := !priority + 2
  end priorities;
  fun tok ->
    assert (Hashtbl.mem ptable tok);
    Hashtbl.find ptable tok

let with_priority env op f =
  let _, prio = get_priority op in
  let env = { env with priority = prio } in
  f env

let with_base_priority env f =
  let env = { env with priority = 0 } in
  f env

(*****************************************************************************)
(* References *)
(*****************************************************************************)

let ref_opt env =
  match L.token env.file env.lb with
  | Tamp when env.mode = FileInfo.Mstrict ->
      error env "Don't use references!";
      true
  | Tamp ->
      true
  | _ ->
      L.back env.lb;
      false

(*****************************************************************************)
(* Identifiers *)
(*****************************************************************************)

let xhp_identifier env =
  (match L.xhpname env.file env.lb with
  | Txhpname ->
      Pos.make env.file env.lb, ":"^Lexing.lexeme env.lb
  | _ ->
      error_expect env "identifier";
      Pos.make env.file env.lb, "*Unknown*"
  )

(* identifier *)
let identifier env =
  match L.token env.file env.lb with
  | Tword ->
      let pos = Pos.make env.file env.lb in
      let name = Lexing.lexeme env.lb in
      pos, name
  | Tcolon ->
      let start = Pos.make env.file env.lb in
      let end_, name = xhp_identifier env in
      Pos.btw start end_, name
  | _ ->
      error_expect env "identifier";
      Pos.make env.file env.lb, "*Unknown*"

(* $variable *)
let variable env =
  match L.token env.file env.lb with
  | Tlvar ->
      Pos.make env.file env.lb, Lexing.lexeme env.lb
  | _ ->
      error_expect env "variable";
      Pos.make env.file env.lb, "$_" (* SpecialIdents.placeholder *)

(* &$variable *)
let ref_variable env =
  let is_ref = ref_opt env in
  (variable env, is_ref)

(* &...$arg *)
let ref_param env =
  let is_ref = ref_opt env in
  let is_variadic = match L.token env.file env.lb with
    | Tellipsis -> true
    | _ -> L.back env.lb; false
  in
  let var = variable env in
  is_ref, is_variadic, var

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rec program ?(elaborate_namespaces = true) file content =
  L.comment_list := [];
  L.fixmes := Utils.IMap.empty;
  let lb = Lexing.from_string content in
  let env = init_env file lb in
  let ast, file_mode = header env in
  let comments = !L.comment_list in
  let fixmes = !L.fixmes in
  L.comment_list := [];
  L.fixmes := Utils.IMap.empty;
  Parser_heap.HH_FIXMES.add env.file fixmes;
  if !(env.errors) <> []
  then Errors.parsing_error (List.hd (List.rev !(env.errors)));
  let ast = if elaborate_namespaces
    then Namespaces.elaborate_defs ast
    else ast in
  {file_mode; comments; ast}

(*****************************************************************************)
(* Hack headers (strict, decl, partial) *)
(*****************************************************************************)

and header env =
  let file_type, head = get_header env in
  match file_type, head with
  | FileInfo.PhpFile, _
  | _, Some FileInfo.Mdecl ->
      let env = { env with mode = FileInfo.Mdecl } in
      let attr = [] in
      let result = ignore_toplevel ~attr [] env (fun x -> x = Teof) in
      expect env Teof;
      result, head
  | _, Some mode ->
      let result = toplevel [] { env with mode = mode } (fun x -> x = Teof) in
      expect env Teof;
      result, head
  | _ ->
      [], head

and get_header env =
  match L.header env.file env.lb with
  | `error -> FileInfo.HhFile, None
  | `default_mode -> FileInfo.HhFile, Some FileInfo.Mpartial
  | `php_decl_mode -> FileInfo.PhpFile, Some FileInfo.Mdecl
  | `php_mode -> FileInfo.PhpFile, None
  | `explicit_mode ->
      let _token = L.token env.file env.lb in
      (match Lexing.lexeme env.lb with
      | "strict" when !(Ide.is_ide_mode) ->
          FileInfo.HhFile, Some FileInfo.Mpartial
      | "strict" -> FileInfo.HhFile, Some FileInfo.Mstrict
      | ("decl"|"only-headers") -> FileInfo.HhFile, Some FileInfo.Mdecl
      | "partial" -> FileInfo.HhFile, Some FileInfo.Mpartial
      | _ ->
          error env
 "Incorrect comment; possible values include strict, decl, partial or empty";
          FileInfo.HhFile, Some FileInfo.Mdecl
      )

(*****************************************************************************)
(* Decl mode *)
(*****************************************************************************)

and ignore_toplevel ~attr acc env terminate =
  match L.token env.file env.lb with
  | x when terminate x || x = Teof ->
      L.back env.lb;
      acc
  | Tltlt ->
      (* Parsing attribute << .. >> *)
      let attr = attribute_remain env in
      ignore_toplevel ~attr acc env terminate
  | Tlcb ->
      let acc = ignore_toplevel ~attr acc env terminate in
      ignore_toplevel ~attr acc env terminate
  | Tquote ->
      let pos = Pos.make env.file env.lb in
      let abs_pos = env.lb.Lexing.lex_curr_pos in
      ignore (expr_string env pos abs_pos);
      ignore_toplevel ~attr acc env terminate
  | Tdquote ->
      let pos = Pos.make env.file env.lb in
      ignore (expr_encapsed env pos);
      ignore_toplevel ~attr acc env terminate
  | Theredoc ->
      ignore (expr_heredoc env);
      ignore_toplevel ~attr acc env terminate
  | Tlt when is_xhp env ->
      ignore (xhp env);
      ignore_toplevel ~attr acc env terminate
  | Tword ->
      (match Lexing.lexeme env.lb with
      | "function" ->
          (match L.token env.file env.lb with
          | Tword ->
              L.back env.lb;
              let def = toplevel_word ~attr env "function" in
              ignore_toplevel ~attr:[] (def @ acc) env terminate
          (* function &foo(...), we still want them in decl mode *)
          | Tamp ->
            (match L.token env.file env.lb with
            | Tword ->
                L.back env.lb;
                let def = toplevel_word ~attr env "function" in
                ignore_toplevel ~attr:[] (def @ acc) env terminate
            | _ ->
              ignore_toplevel ~attr acc env terminate
            )
          | _ ->
              ignore_toplevel ~attr acc env terminate
          )
      | "abstract" | "final"
      | "class"| "trait" | "interface"
      | "namespace"
      | "async" | "newtype"| "type"| "const" ->
          (* Parsing toplevel declarations (class, function etc ...) *)
          let def = toplevel_word ~attr env (Lexing.lexeme env.lb) in
          ignore_toplevel ~attr:[] (def @ acc) env terminate
      | _ -> ignore_toplevel ~attr acc env terminate
      )
  | Tclose_php ->
      error env "Hack does not allow the closing ?> tag";
      acc
  | _ -> ignore_toplevel ~attr acc env terminate

(*****************************************************************************)
(* Toplevel statements. *)
(*****************************************************************************)

and toplevel acc env terminate =
  match L.token env.file env.lb with
  | x when terminate x || x = Teof ->
      L.back env.lb;
      List.rev acc
  | Tsc ->
      (* Ignore extra semicolons at toplevel (important so we don't yell about
       * them in strict mode). *)
      toplevel acc env terminate
  | Tltlt ->
      (* Parsing attribute << .. >> *)
      let attr = attribute_remain env in
      let _ = L.token env.file env.lb in
      let def = toplevel_word ~attr env (Lexing.lexeme env.lb) in
      toplevel (def @ acc) env terminate
  | Tword ->
      (* Parsing toplevel declarations (class, function etc ...) *)
      let def = toplevel_word ~attr:[] env (Lexing.lexeme env.lb) in
      toplevel (def @ acc) env terminate
  | Tclose_php ->
      error env "Hack does not allow the closing ?> tag";
      List.rev acc
  | _ ->
      (* All the other statements. *)
      let pos = Pos.make env.file env.lb in
      L.back env.lb;
      let error_state = !(env.errors) in
      let stmt = Stmt (statement env) in
      check_toplevel env pos;
      if error_state != !(env.errors)
      then ignore_toplevel ~attr:[] (stmt :: acc) env terminate
      else toplevel (stmt :: acc) env terminate

and toplevel_word ~attr env = function
  | "abstract" ->
    let final = (match L.token env.file env.lb with
      | Tword when Lexing.lexeme env.lb = "final" -> true
      | _ -> begin L.back env.lb; false end
    ) in
    expect_word env "class";
    let class_ = class_ ~attr ~final ~kind:Cabstract env in
    [Class class_]
  | "final" ->
      expect_word env "class";
      let class_ = class_ ~attr ~final:true ~kind:Cnormal env in
      [Class class_]
  | "class" ->
      let class_ = class_ ~attr ~final:false ~kind:Cnormal env in
      [Class class_]
  | "trait" ->
      let class_ = class_ ~attr ~final:false ~kind:Ctrait env in
      [Class class_]
  | "interface" ->
      let class_ = class_ ~attr ~final:false ~kind:Cinterface env in
      [Class class_]
  | "enum" ->
      let class_ = enum_ ~attr env in
      [Class class_]
  | "async" ->
      expect_word env "function";
      let fun_ = fun_ ~attr ~sync:FDeclAsync env in
      [Fun fun_]
  | "function" ->
      let fun_ = fun_ ~attr ~sync:FDeclSync env in
      [Fun fun_]
  | "newtype" ->
      let typedef_ = typedef ~attr ~is_abstract:true env in
      [Typedef typedef_]
  | "type" ->
      let typedef_ = typedef ~attr ~is_abstract:false env in
      [Typedef typedef_]
  | "namespace" ->
      let id, body = namespace env in
      [Namespace (id, body)]
  | "use" ->
      let usel = namespace_use_list env [] in
      [NamespaceUse usel]
  | "const" ->
      let consts = class_const_def env in
      (match consts with
      | Const (h, cstl) ->
          List.map (fun (x, y) -> Constant {
            cst_mode = env.mode;
            cst_kind = Cst_const;
            cst_name = x;
            cst_type = h;
            cst_value = y;
            cst_namespace = Namespace_env.empty;
          }) cstl
      | _ -> assert false)
  | r when is_import r ->
      let pos = Pos.make env.file env.lb in
      let e = expr_import r env pos in
      expect env Tsc;
      [Stmt (Expr e)]
  | _ ->
      let pos = Pos.make env.file env.lb in
      L.back env.lb;
      let stmt = statement env in
      check_toplevel env pos;
      [define_or_stmt env stmt]

and define_or_stmt env = function
  | Expr (_, Call ((_, Id (_, "define")), [(_, String name); value], [])) ->
      Constant {
      cst_mode = env.mode;
      cst_kind = Cst_define;
      cst_name = name;
      cst_type = None;
      cst_value = value;
      cst_namespace = Namespace_env.empty;
    }
  | stmt ->
      Stmt stmt

(*****************************************************************************)
(* Attributes: <<_>> *)
(*****************************************************************************)

(* <<_>> *)
and attribute env =
  if look_ahead env (fun env -> L.token env.file env.lb = Tltlt)
  then begin
    expect env Tltlt;
    attribute_remain env;
  end
  else []

(* _>> *)
and attribute_remain env =
  match L.token env.file env.lb with
  | Tword ->
      (* Temporary backwards compat for renaming these attributes.
       * TODO #4890694 remove this. *)
      let attr_compat = function
        | "ConsistentConstruct" -> "__ConsistentConstruct"
        | "Override" -> "__Override"
        | "UNSAFE_Construct" -> "__UNSAFE_Construct"
        | x -> x in
      let pos = Pos.make env.file env.lb in
      let ua_name = pos, attr_compat (Lexing.lexeme env.lb) in
      let ua_params = attribute_parameters env in
      let attr = { ua_name; ua_params } in
      attr :: attribute_list_remain env
  | _ ->
      error_expect env "attribute name";
      []

(* empty | (parameter_list) *)
and attribute_parameters env =
  match L.token env.file env.lb with
  | Tlp -> expr_list_remain env
  | _ -> L.back env.lb; []

(* ,_,>> *)
and attribute_list_remain env =
  match L.token env.file env.lb with
  | Tgtgt -> []
  | Tcomma -> attribute_remain env
  | _ ->
      error_expect env ">>";
      []

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

and fun_ ~attr ~(sync:fun_decl_kind) env =
  let is_ref = ref_opt env in
  if is_ref && sync = FDeclAsync
    then error env ("Asynchronous function cannot return reference");
  let name = identifier env in
  let tparams = class_params env in
  let params = parameter_list env in
  let ret = hint_return_opt env in
  let is_generator, body_stmts = function_body env in
  { f_name = name;
    f_tparams = tparams;
    f_params = params;
    f_ret = ret;
    f_ret_by_ref = is_ref;
    f_body = body_stmts;
    f_user_attributes = attr;
    f_fun_kind = fun_kind sync is_generator;
    f_mode = env.mode;
    f_mtime = 0.0;
    f_namespace = Namespace_env.empty;
  }

(*****************************************************************************)
(* Classes *)
(*****************************************************************************)

and class_ ~attr ~final ~kind env =
  let cname       = identifier env in
  let is_xhp      = (snd cname).[0] = ':' in
  let tparams     = class_params env in
  let cextends    =
    if kind = Ctrait then []
    else class_extends ~single:(kind <> Cinterface) env in
  let cimplements = class_implements kind env in
  let cbody       = class_body env in
  let result =
    { c_mode            = env.mode;
      c_final           = final;
      c_kind            = kind;
      c_is_xhp          = is_xhp;
      c_implements      = cimplements;
      c_tparams         = tparams;
      c_user_attributes = attr;
      c_name            = cname;
      c_extends         = cextends;
      c_body            = cbody;
      c_namespace       = Namespace_env.empty;
      c_enum            = None;
    }
  in
  class_implicit_fields result

(*****************************************************************************)
(* Enums *)
(*****************************************************************************)

and enum_base_ty env =
  expect env Tcolon;
  let h = hint env in
  h

and enum_ ~attr env =
  let cname       = identifier env in
  let basety      = enum_base_ty env in
  let constraint_ = typedef_constraint env in
  let cbody       = enum_body env in
  let result =
    { c_mode            = env.mode;
      c_final           = false;
      c_kind            = Cenum;
      c_is_xhp          = false;
      c_implements      = [];
      c_tparams         = [];
      c_user_attributes = attr;
      c_name            = cname;
      c_extends         = [];
      c_body            = cbody;
      c_namespace       = Namespace_env.empty;
      c_enum            = Some
        { e_base       = basety;
          e_constraint = constraint_;
        }
    }
  in
  result

(* { ... *)
and enum_body env =
  expect env Tlcb;
  enum_defs env

and enum_defs env =
  match peek env with
  (* ... } *)
  | Trcb ->
      drop env;
      []
  | Tword ->
    let const = class_const env in
    let elem = Const (None, [const]) in
    expect env Tsc;
    let rest = enum_defs env in
    elem :: rest
  | _ ->
    error_expect env "enum const declaration";
    []


(*****************************************************************************)
(* Extends/Implements *)
(*****************************************************************************)

and class_extends ~single env =
  match L.token env.file env.lb with
  | Tword ->
      (match Lexing.lexeme env.lb with
      | "extends" -> if single then [class_hint env] else class_extends_list env
      | "implements" -> L.back env.lb; []
      | s -> error env ("Expected: extends; Got: "^s); []
      )
  | Tlcb ->
      L.back env.lb;
      []
  | _ ->
      error_expect env "{";
      []

and class_implements kind env =
  match L.token env.file env.lb with
  | Tword ->
      (match Lexing.lexeme env.lb with
      | "implements" ->
         let impl = class_extends_list env in
         if kind = Cinterface then begin
           error env "Expected: extends; Got implements"; []
         end else
           impl
      | "extends" -> L.back env.lb; []
      | s -> error env ("Expected: implements; Got: "^s); []
      )
  | Tlcb ->
      L.back env.lb;
      []
  | _ ->
      error_expect env "{";
      []

and class_extends_list env =
  let error_state = !(env.errors) in
  let c = class_hint env in
  match L.token env.file env.lb with
  | Tlcb ->
      L.back env.lb; [c]
  | Tcomma ->
      if !(env.errors) != error_state
      then [c]
      else c :: class_extends_list env
  | Tword ->
      (match Lexing.lexeme env.lb with
      | "implements" | "extends" -> L.back env.lb; [c]
      | _ -> error_expect env "{"; []
      )
  | _ -> error_expect env "{"; []

(*****************************************************************************)
(* Class parameters class A<T as X ..> *)
(*****************************************************************************)

and class_params env =
  match L.token env.file env.lb with
  | Tlt -> class_param_list env
  | _ -> L.back env.lb; []

and class_param_list env =
  let error_state = !(env.errors) in
  let cst = class_param env in
  match L.gt_or_comma env.file env.lb with
  | Tgt ->
      [cst]
  | Tcomma ->
      if !(env.errors) != error_state
      then [cst]
      else cst :: class_param_list env
  | _ ->
      error_expect env ">";
      [cst]

and class_param env =
  match L.token env.file env.lb with
  | Tplus ->
      if L.token env.file env.lb <> Tword
      then class_param_error env
      else
        let parameter_name, parameter_constraint = class_param_name env in
        Covariant, parameter_name, parameter_constraint
  | Tminus ->
      if L.token env.file env.lb <> Tword
      then class_param_error env
      else
        let parameter_name, parameter_constraint = class_param_name env in
        Contravariant, parameter_name, parameter_constraint
  | Tword ->
      let parameter_name, parameter_constraint = class_param_name env in
      let variance = Invariant in
      variance, parameter_name, parameter_constraint
  | _ ->
      class_param_error env

and class_param_error env =
  error_expect env "type parameter";
  let parameter_name = Pos.make env.file env.lb, "T*unknown*" in
  Invariant, parameter_name, None

and class_param_name env =
  let parameter_name = Pos.make env.file env.lb, Lexing.lexeme env.lb in
  let parameter_constraint = class_parameter_constraint env in
  parameter_name, parameter_constraint

and class_parameter_constraint env =
  match L.token env.file env.lb with
  | Tword when Lexing.lexeme env.lb = "as" -> Some (Constraint_as, hint env)
  | Tword when Lexing.lexeme env.lb = "super" ->
      Some (Constraint_super, hint env)
  | _ -> L.back env.lb; None

(*****************************************************************************)
(* Class hints (A<T> etc ...) *)
(*****************************************************************************)

and class_hint env =
  let pname = identifier env in
  class_hint_with_name env pname

and class_hint_with_name env pname =
  let params = class_hint_params env in
  (fst pname), Happly (pname, params)

and class_hint_params env =
  match L.token env.file env.lb with
  | Tlt -> class_hint_param_list env
  | _ -> L.back env.lb; []

and class_hint_param_list env =
  let error_state = !(env.errors) in
  let h = hint env in
  match L.gt_or_comma env.file env.lb with
  | Tgt ->
      [h]
  | Tcomma ->
      if !(env.errors) != error_state
      then [h]
      else h :: class_hint_param_list env
  | _ ->
      error_expect env ">"; [h]

(*****************************************************************************)
(* Type hints: int, ?int, A<T>, array<...> etc ... *)
(*****************************************************************************)

and hint env =
  match L.token env.file env.lb with
  (* ?_ *)
  | Tqm ->
      let start = Pos.make env.file env.lb in
      let e = hint env in
      Pos.btw start (fst e), Hoption e
  (* A<_> *)(* :XHPNAME *)
  | Tword when Lexing.lexeme env.lb = "shape" ->
      let pos = Pos.make env.file env.lb in
      pos, Hshape (hint_shape_field_list env pos)
  | Tword | Tcolon when Lexing.lexeme env.lb <> "function" ->
      L.back env.lb;
      hint_apply_or_access env []
  | Tword ->
      let h = hint_function env in
      error_at env (fst h) "Function hints must be parenthesized";
      h
  (* (_) | (function(_): _) *)
  | Tlp ->
      let start_pos = Pos.make env.file env.lb in
      hint_paren start_pos env
  (* @_ *)
  | Tat ->
      let start = Pos.make env.file env.lb in
      let h = hint env in
      Pos.btw start (fst h), snd h
  | _ ->
      error_expect env "type";
      let pos = Pos.make env.file env.lb in
      pos, Happly ((pos, "*Unknown*"), [])

and hint_apply_or_access env id_list =
  match L.token env.file env.lb with
    (* A | :XHPNAME *)
    | Tword | Tcolon ->
        L.back env.lb;
        hint_apply_or_access_remainder env (identifier env :: id_list)
    | _ ->
        error_expect env "identifier";
        let pos = Pos.make env.file env.lb in
        pos, Happly ((pos, "*Unknown*"), [])

and hint_apply_or_access_remainder env id_list =
  match L.token env.file env.lb with
    (* ...::... *)
    | Tcolcol -> hint_apply_or_access env id_list
    (* ...<_> | ... *)
    | _ ->
        L.back env.lb;
        begin match List.rev id_list with
          | [id] ->
              let params = class_hint_params env in
              let id = List.hd id_list in
              fst id, Happly (id, params)
          | id1 :: id2 :: ids ->
              let pos1 = fst id1 in
              let pos2 = List.fold_left (fun _acc (p, _) -> p) (fst id2) ids in
              Pos.btw pos1 pos2, Haccess (id1, id2, ids)
          | [] ->
              error_expect env "identifier";
              let pos = Pos.make env.file env.lb in
              pos, Happly ((pos, "*Unknown*"), [])
        end

(* (_) | (function(_): _) *)
and hint_paren start env =
  match L.token env.file env.lb with
  | Tword when Lexing.lexeme env.lb = "function" ->
      let h = hint_function env in
      if L.token env.file env.lb <> Trp
      then error_at env (fst h) "Function hints must be parenthesized";
      Pos.btw start (Pos.make env.file env.lb), (snd h)
  | _ ->
      L.back env.lb;
      let hintl = hint_list env in
      let end_ = Pos.make env.file env.lb in
      let pos = Pos.btw start end_ in
      match hintl with
      | []  -> assert false
      | [_] ->
          error_at env pos "Tuples of one element are not allowed";
          pos, Happly ((pos, "*Unknown*"), [])
      | hl  -> pos, Htuple hl

and hint_list env =
  let error_state = !(env.errors) in
  let h = hint env in
  match L.token env.file env.lb with
  | Trp ->
      [h]
  | Tcomma ->
      if !(env.errors) != error_state
      then [h]
      else h :: hint_list_remain env
  | _ ->
      error_expect env ">"; [h]

and hint_list_remain env =
  match L.token env.file env.lb with
  | Trp -> []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let h = hint env in
      match L.token env.file env.lb with
      | Trp ->
          [h]
      | Tcomma ->
          if !(env.errors) != error_state
          then [h]
          else h :: hint_list_remain env
      | _ ->
          error_expect env ">"; [h]

(*****************************************************************************)
(* Function hint (function(_): _) *)
(*****************************************************************************)

(* function(_): _ *)
and hint_function env =
  let start = Pos.make env.file env.lb in
  expect env Tlp;
  let params, has_dots = hint_function_params env in
  let ret = hint_return env in
  Pos.btw start (fst ret), Hfun (params, has_dots, ret)

(* (parameter_1, .., parameter_n) *)
and hint_function_params env =
  match L.token env.file env.lb with
  | Trp ->
      ([], false)
  | Tellipsis ->
      hint_function_params_close env;
      ([], true)
  | _ ->
      L.back env.lb;
      hint_function_params_remain env

(* ) | ,) *)
and hint_function_params_close env =
  match L.token env.file env.lb with
  | Trp ->
      ()
  | Tcomma ->
      expect env Trp
  | _ ->
      error_expect env ")";
      ()

(* _, parameter_list | _) | ...) | ...,) *)
and hint_function_params_remain env =
  let error_state = !(env.errors) in
  let h = hint env in
  match L.token env.file env.lb with
  | Tcomma ->
      if !(env.errors) != error_state
      then ([h], false)
      else
        let hl, has_dots = hint_function_params env in
        (h :: hl, has_dots)
  | Trp ->
      ([h], false)
  | Tellipsis ->
      hint_function_params_close env;
      ([h], true)
  | _ ->
      error_expect env ")";
      ([h], false)

and xhp_enum_decl_list env =
  match L.token env.file env.lb with
  | Trcb -> []
  | _ -> L.back env.lb; xhp_enum_decl_list_remain env

and xhp_enum_decl_list_remain env =
  let error_state = !(env.errors) in
  let v = xhp_enum_decl_value env in
  match L.token env.file env.lb with
  | Trcb ->
      [v]
  | Tcomma ->
      if !(env.errors) != error_state
        then [v]
        else v :: xhp_enum_decl_list env
  | _ ->
      error_expect env "}"; [v]

and xhp_enum_decl_value env =
  let tok = L.token env.file env.lb in
  let pos = Pos.make env.file env.lb in
  match tok with
  | Tint ->
      let tok_value = Lexing.lexeme env.lb in
      pos, Int (pos, tok_value)
  | Tquote ->
      let absolute_pos = env.lb.Lexing.lex_curr_pos in
      expr_string env pos absolute_pos
  | Tdquote ->
      expr_encapsed env pos
  | _ ->
      error_expect env "integer literal or string literal";
      pos, Null

(* : _ *)
and hint_return env =
  expect env Tcolon;
  hint env

and hint_return_opt env =
  match L.token env.file env.lb with
  | Tcolon -> Some (hint env)
  | _ -> L.back env.lb; None

(*****************************************************************************)
(* Class statements *)
(*****************************************************************************)

(* { ... *)
and class_body env =
  let error_state = !(env.errors) in
  expect env Tlcb;
  if error_state != !(env.errors)
  then L.look_for_open_cb env.lb;
  class_defs env

and class_defs env =
  match L.token env.file env.lb with
  (* ... } *)
  | Trcb ->
      []
  (* xhp_format | const | use *)
  | Tword ->
      let word = Lexing.lexeme env.lb in
      class_toplevel_word env word
  | Tltlt ->
  (* variable | method | type const*)
      L.back env.lb;
      let error_state = !(env.errors) in
      let m = class_member_def env in
      if !(env.errors) != error_state
      then [m]
      else m :: class_defs env
  | _ ->
      error_expect env "class member";
      let start = Pos.make env.file env.lb in
      look_for_next_method start env;
      let _ = L.token env.file env.lb in
      let word = Lexing.lexeme env.lb in
      class_toplevel_word env word

and class_toplevel_word env word =
  match word with
  | "category" | "children" ->
      xhp_format env;
      class_defs env
  | "const" ->
      let error_state = !(env.errors) in
      let def =
        match try_typeconst_def env ~is_abstract:false with
        | Some tconst -> tconst
        | None -> class_const_def env
      in
      if !(env.errors) != error_state
      then [def]
      else def :: class_defs env
  | "use" ->
      let traitl = class_use_list env in
      traitl @ class_defs env
  | "require" ->
      let traitl = trait_require env in
      traitl @ class_defs env
  | "attribute" ->
      let start = Pos.make env.file env.lb in
      let error_state = !(env.errors) in
      let m = xhp_attr_list env in
      if !(env.errors) != error_state
      then look_for_next_method start env;
      m @ class_defs env
  | "abstract" ->
    (match try_abstract_const env with
      | Some ac -> ac :: class_defs env
      | None -> on_class_member_word env)
  | "public" | "protected" | "private" | "final" | "static"  ->
    on_class_member_word env
  | _ ->
      error_expect env "modifier";
      []

and on_class_member_word env =
  (* variable | method | type const*)
  L.back env.lb;
  let start = Pos.make env.file env.lb in
  let error_state = !(env.errors) in
  let m = class_member_def env in
  if !(env.errors) != error_state
  then look_for_next_method start env;
  m :: class_defs env

and look_for_next_method previous_pos env =
  match L.token env.file env.lb with
  | Teof -> ()
  | Trcb -> ()
  | Tword ->
      (match Lexing.lexeme env.lb with
      | "abstract"| "public" | "protected"
      | "private" | "final" | "static" when not (peek_check_word env "const") ->
          let pos = Pos.make env.file env.lb in
          if Pos.compare pos previous_pos = 0
          then (* we are stuck in a circle *)
            look_for_next_method pos env
          else
            (L.back env.lb; ())
      | _ -> look_for_next_method previous_pos env
      )
  | _ -> look_for_next_method previous_pos env

(*****************************************************************************)
(* Use (for traits) *)
(*****************************************************************************)

and class_use_list env =
  let error_state = !(env.errors) in
  let cst = ClassUse (class_hint env) in

  match L.token env.file env.lb with
  | Tsc ->
      [cst]
  | Tcomma ->
      if !(env.errors) != error_state
      then [cst]
      else cst :: class_use_list env
  | _ ->
      error_expect env ";"; [cst]

and trait_require env =
  match L.token env.file env.lb with
  | Tword ->
    let req_type = Lexing.lexeme env.lb in
    let ret = (match req_type with
      | "implements" -> [ClassTraitRequire (MustImplement, class_hint env)]
      | "extends" -> [ClassTraitRequire (MustExtend, class_hint env)]
      | _ -> error env "Expected: implements or extends"; []
    ) in
    (match L.token env.file env.lb with
      | Tsc -> ret
      | _ -> error_expect env ";"; [])
  | _ -> error env "Expected: implements or extends"; []

(*****************************************************************************)
(* Class xhp_format *)
(*
 * within a class body -->
 *    children ...;
 *    attribute ...;
 *    category ...;
 *)
(*****************************************************************************)

and xhp_format env =
  match L.token env.file env.lb with
  | Tsc -> ()
  | Teof ->
      error_expect env "end of XHP category/attribute/children declaration";
      ()
  | Tquote ->
      let pos = Pos.make env.file env.lb in
      let abs_pos = env.lb.Lexing.lex_curr_pos in
      ignore (expr_string env pos abs_pos);
      xhp_format env
  | Tdquote ->
      let pos = Pos.make env.file env.lb in
      ignore (expr_encapsed env pos);
      xhp_format env
  | x ->
      xhp_format env

(*****************************************************************************)
(* Class constants *)
(*
 *  within a class body -->
 *    const ...;
 *)
(*****************************************************************************)

(* Is "abstract" followed by "const"?
   abstract const _ X; *)
and try_abstract_const env =
    try_parse env begin fun env ->
      match L.token env.file env.lb with
        | Tword when Lexing.lexeme env.lb = "const" ->
            (match try_typeconst_def env ~is_abstract:true with
            | Some tconst -> Some tconst
            | None ->
                let h = class_const_hint env in
                let id = identifier env in
                expect env Tsc;
                Some (AbsConst (h, id))
            )
        | _ -> None
    end

and try_typeconst_def env ~is_abstract =
  try_parse env begin fun env ->
    match L.token env.file env.lb with
    | Tword when Lexing.lexeme env.lb = "type" && (peek env) = Tword ->
        Some (TypeConst (typeconst_def env ~is_abstract))
    | _ -> None
  end

(* const_hint const_name1 = value1, ..., const_name_n = value_n; *)
and class_const_def env =
  let h = class_const_hint env in
  let consts = class_const_list env in
  Const (h, consts)

(* const _ X = ...; *)
and class_const_hint env =
  if class_const_has_hint env
  then Some (hint env)
  else None

(* Determines if there is a type-hint by looking ahead. *)
and class_const_has_hint env =
  look_ahead env begin fun env ->
    match L.token env.file env.lb with
    (* const_name = ... | hint_name const_name = ... *)
    | Tword ->
      (* If we see 'name =' or 'name;', there is no type hint *)
      let tok = L.token env.file env.lb in
      (tok <> Teq && tok <> Tsc)
    | _ -> true
  end

and class_const_list env =
  let error_state = !(env.errors) in
  let cst = class_const env in
  match L.token env.file env.lb with
  | Tsc ->
      [cst]
  | Tcomma ->
      if !(env.errors) != error_state
      then [cst]
      else cst :: class_const_list_remain env
  | _ ->
      error_expect env ";"; [cst]

and class_const_list_remain env =
  match L.token env.file env.lb with
  | Tsc -> []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let cst = class_const env in
      match L.token env.file env.lb with
      | Tsc ->
          [cst]
      | Tcomma ->
          if !(env.errors) != error_state
          then [cst]
          else  cst :: class_const_list_remain env
      | _ ->
          error_expect env ";"; [cst]

(* const_name = const_value *)
and class_const env =
  let id = identifier env in
  expect env Teq;
  let e = expr env in
  id, e

(*****************************************************************************)
(* Modifiers *)
(*****************************************************************************)

and mandatory_modifier_list env =
  match L.token env.file env.lb with
  | Tword ->
      let word = Lexing.lexeme env.lb in
      (match modifier_word env word with
      | None -> error_expect env "modifier"; []
      | Some v -> v :: optional_modifier_list env
      )
  | _ ->
      error_expect env "modifier"; []

and optional_modifier_list env =
  match L.token env.file env.lb with
  | Tword ->
      let word = Lexing.lexeme env.lb in
      (match modifier_word env word with
      | None -> L.back env.lb; []
      | Some v -> v :: optional_modifier_list env
      )
  | _ ->
      L.back env.lb; []

and modifier_word env = function
  | "final"     -> Some Final
  | "static"    ->
      (* We need to look ahead to make sure we are not looking at a type access
       *
       * class C {
       *   public static $x; // a static var
       *   public static::T $y; // an instance var with type static::T
       * }
       *)
      if peek env = Tcolcol
      then None
      else Some Static
  | "abstract"  -> Some Abstract
  | "private"   -> Some Private
  | "public"    -> Some Public
  | "protected" -> Some Protected
  | _ -> None

(*****************************************************************************)
(* Class variables/methods. *)
(*
 *  within a class body -->
 *    modifier_list ...;
 *)
(*****************************************************************************)

and class_member_def env =
  let attrs = attribute env in
  let modifier_start = Pos.make env.file env.lb in
  let modifiers = mandatory_modifier_list env in
  let modifier_end = Pos.make env.file env.lb in
  let modifier_pos = Pos.btw modifier_start modifier_end in
  check_modifiers env modifier_pos modifiers;
  match L.token env.file env.lb with
  (* modifier_list $_ *)
  | Tlvar ->
      L.back env.lb;
      check_not_final env modifier_pos modifiers;
      let cvars = class_var_list env in
      ClassVars (modifiers, None, cvars)
  | Tword ->
      let word = Lexing.lexeme env.lb in
      class_member_word env ~modifiers ~attrs word
  | _ ->
      L.back env.lb;
      check_visibility env modifier_pos modifiers;
      check_not_final env modifier_pos modifiers;
      let h = hint env in
      let cvars = class_var_list env in
      ClassVars (modifiers, Some h, cvars)

(*****************************************************************************)
(* Class variables *)
(*
 *  within a class body -->
 *    modifier_list $x;
 *    modifier_list hint $x;
 *)
(*****************************************************************************)

and class_var_list env =
  let error_state = !(env.errors) in
  let cvar = class_var env in
  if !(env.errors) != error_state
  then [cvar]
  else cvar :: class_var_list_remain env

and class_var_list_remain env =
  match L.token env.file env.lb with
  | Tsc ->
      []
  | Tcomma ->
      (match L.token env.file env.lb with
      | Tsc ->
          []
      | _ ->
          L.back env.lb;
          let error_state = !(env.errors) in
          let var = class_var env in
          if !(env.errors) != error_state
          then [var]
          else var :: class_var_list_remain env
      )
  | _ -> error_expect env ";"; []

and class_var env =
  let pos, name = variable env in
  let name = class_var_name name in
  let default = parameter_default env in
  (pos, name), default

and class_var_name name =
    String.sub name 1 (String.length name - 1)

and xhp_attr env =
  let maybe_use, maybe_enum = (match L.token env.file env.lb with
    | Tcolon ->
        L.back env.lb; (try_xhp_attr_use env, None)
    | Tword when Lexing.lexeme env.lb = "enum" ->
        L.back env.lb; (None, try_xhp_enum_hint env)
    | _ ->
        L.back env.lb; (None, None)) in
  match maybe_use with
    | Some x -> x
    | None ->
        begin
          let h = (match maybe_enum with
            | Some x -> None
            | None -> Some (hint env)) in
          let ident = xhp_identifier env in
          let default = parameter_default env in
          let is_required = (match L.token env.file env.lb with
            | Trequired -> true
            | _ -> L.back env.lb; false) in
          XhpAttr ([], h, [ident, default], is_required, maybe_enum)
        end

and xhp_attr_list env =
  let error_state = !(env.errors) in
  let a = xhp_attr env in
  if !(env.errors) != error_state
    then [a]
    else [a] @ xhp_attr_list_remain env

and xhp_attr_list_remain env =
  match L.token env.file env.lb with
  | Tsc ->
      []
  | Tcomma ->
      (match L.token env.file env.lb with
      | Tsc ->
          []
      | _ ->
          L.back env.lb;
          xhp_attr_list env)
  | _ -> error_expect env ";"; []

(*****************************************************************************)
(* Methods *)
(*
 *  within a class body -->
 *    modifier_list async function ...
 *    modifier_list function ...
 *)
(*****************************************************************************)

and class_member_word env ~attrs ~modifiers = function
  | "async" ->
      expect_word env "function";
      let is_ref = ref_opt env in
      if is_ref
        then error env ("Asynchronous function cannot return reference");
      let fun_name = identifier env in
      let method_ =
        method_ env ~modifiers ~attrs ~sync:FDeclAsync is_ref fun_name in
      Method method_
  | "function" ->
      let is_ref = ref_opt env in
      let fun_name = identifier env in
      let method_ =
        method_ env ~modifiers ~attrs ~sync:FDeclSync is_ref fun_name in
      Method method_
  | _ ->
      L.back env.lb;
      let h = hint env in
      let cvars =
        match L.token env.file env.lb with
        | Tword when Lexing.lexeme env.lb = "function" ->
            error env ("Expected variable. "^
              "Perhaps you meant 'function (...): return-type'?");
            []
        | _ -> L.back env.lb; class_var_list env
      in ClassVars (modifiers, Some h, cvars)

and typeconst_def env ~is_abstract =
  let pname = identifier env in
  let constr = typedef_constraint env in
  let type_ = match L.token env.file env.lb with
    | Teq -> Some (hint env)
    | _ -> L.back env.lb; None
  in
  expect env Tsc;
  { tconst_abstract = is_abstract;
    tconst_name = pname;
    tconst_constraint = constr;
    tconst_type = type_;
  }

and method_ env ~modifiers ~attrs ~(sync:fun_decl_kind) is_ref pname =
  let pos, name = pname in
  let tparams = class_params env in
  let params = parameter_list env in
  let ret = hint_return_opt env in
  let is_generator, body_stmts = function_body env in
  let ret = method_implicit_return env pname ret in
  if name = "__destruct" && params <> []
  then error_at env pos "Destructor must not have any parameters.";
  { m_name = pname;
    m_tparams = tparams;
    m_params = params;
    m_ret = ret;
    m_ret_by_ref = is_ref;
    m_body = body_stmts;
    m_kind = modifiers;
    m_user_attributes = attrs;
    m_fun_kind = fun_kind sync is_generator;
  }

(*****************************************************************************)
(* Constructor/Destructors special cases. *)
(*****************************************************************************)

and method_implicit_return env (pos, name) ret =
  match name, ret with
  | ("__construct" | "__destruct"), None ->
      Some (pos, Happly((pos, "void"), []))
  | _, Some (_, Happly ((_, "void"), [])) -> ret
  | "__construct", Some _ ->
      error_at env pos "Constructor return type must be void or elided.";
      None
  | "__destruct", Some _ ->
      error_at env pos "Destructor return type must be void or elided.";
      None
  | _ -> ret

(*****************************************************************************)
(* Implicit class fields __construct(public int $x). *)
(*****************************************************************************)

and class_implicit_fields class_ =
  let class_body = method_implicit_fields class_.c_body in
  { class_ with c_body = class_body }

and method_implicit_fields members =
  match members with
  | [] -> []
  | Method ({ m_name = _, "__construct"; _ } as m) :: rl ->
      let fields, assigns = param_implicit_fields m.m_params in
      fields @ Method { m with m_body = assigns @ m.m_body } :: rl
  | x :: rl ->
      x :: method_implicit_fields rl

and param_implicit_fields params =
  match params with
  | [] -> [], []
  | { param_modifier = Some vis; _ } as p :: rl ->
      let member, stmt = param_implicit_field vis p in
      let members, assigns = param_implicit_fields rl in
      member :: members, stmt :: assigns
  | _ :: rl ->
      param_implicit_fields rl

and param_implicit_field vis p =
  (* Building the implicit field (for example: private int $x;) *)
  let pos, name = p.param_id in
  let cvname = pos, class_var_name name in
  let member = ClassVars ([vis], p.param_hint, [cvname, None]) in
  (* Building the implicit assignment (for example: $this->x = $x;) *)
  let this = pos, "$this" in
  let stmt =
    Expr (pos, Binop (Eq None, (pos, Obj_get((pos, Lvar this),
                                             (pos, Id cvname),
                                             OG_nullthrows)),
                      (pos, Lvar p.param_id)))
  in
  member, stmt

(*****************************************************************************)
(* Function/Method bodies. *)
(*****************************************************************************)

and function_body env =
  match L.token env.file env.lb with
  | Tsc ->
    false, []
  | Tlcb ->
    let previous_in_generator = !(env.in_generator) in
    env.in_generator := false;
    let statements = (match env.mode with
      | FileInfo.Mdecl ->
        ignore_body env;
        (* This is a hack for the type-checker to make a distinction
         * Between function foo(); and function foo() {}
         *)
        [Noop]
      | _ ->
        (match statement_list env with
          | [] -> [Noop]
          | x -> x)
    ) in
    let in_generator = !(env.in_generator) in
    env.in_generator := previous_in_generator ;
    in_generator, statements
  | _ ->
    error_expect env "{";
    false, []

and fun_kind sync (has_yield:bool) =
  match sync, has_yield with
    | FDeclAsync, true -> FAsyncGenerator
    | FDeclSync, true -> FGenerator
    | FDeclAsync, false -> FAsync
    | FDeclSync, false -> FSync

and ignore_body env =
  match L.token env.file env.lb with
  | Tlcb -> ignore_body env; ignore_body env
  | Trcb -> ()
  | Tquote ->
    let pos = Pos.make env.file env.lb in
    let abs_pos = env.lb.Lexing.lex_curr_pos in
    ignore (expr_string env pos abs_pos);
    ignore_body env
  | Tdquote ->
    let pos = Pos.make env.file env.lb in
    ignore (expr_encapsed env pos);
    ignore_body env
  | Theredoc ->
    ignore (expr_heredoc env);
    ignore_body env
  | Tword when (Lexing.lexeme env.lb) = "yield" ->
    env.in_generator := true;
    ignore_body env
  | Tword when (Lexing.lexeme env.lb) = "function" && peek env = Tlp ->
    (* this covers the async case as well *)
    let pos = Pos.make env.file env.lb in
    with_ignored_yield env
      (fun () -> ignore (expr_anon_fun env pos ~sync:FDeclSync));
    ignore_body env
  | Tlp ->
    with_ignored_yield env
      (fun () -> ignore (try_short_lambda env));
    ignore_body env
  | Tlt when is_xhp env ->
    ignore (xhp env);
    ignore_body env
  | Teof -> error_expect env "}"; ()
  | _ -> ignore_body env

and with_ignored_yield env fn =
  let previous_in_generator = !(env.in_generator) in
  let () = fn () in
  env.in_generator := previous_in_generator; ()

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

and statement_list env =
  match L.token env.file env.lb with
  | Trcb -> []
  | Tlcb ->
      let block = statement_list env in
      Block block :: statement_list env
  | Tsc ->
      statement_list env
  | Teof ->
      error_expect env "}";
      []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let stmt = statement env in
      if !(env.errors) != error_state
      then L.next_newline_or_close_cb env.lb;
      stmt :: statement_list env

and statement env =
  match L.token env.file env.lb with
  | Tword ->
      let word = Lexing.lexeme env.lb in
      let stmt = statement_word env word in
      stmt
  | Tlcb ->
      Block (statement_list env)
  | Tsc ->
      Noop
  | Tunsafe ->
      Unsafe
  | Tfallthrough ->
      Fallthrough
  | _ ->
      L.back env.lb;
      let e = expr env in
      expect env Tsc;
      Expr e

and statement_word env = function
  | "break"    -> statement_break env
  | "continue" -> statement_continue env
  | "throw"    -> statement_throw env
  | "return"   -> statement_return env
  | "static"   -> statement_static env
  | "print"    -> statement_echo env
  | "echo"     -> statement_echo env
  | "if"       -> statement_if env
  | "do"       -> statement_do env
  | "while"    -> statement_while env
  | "for"      -> statement_for env
  | "switch"   -> statement_switch env
  | "foreach"  -> statement_foreach env
  | "try"      -> statement_try env
  | "function" | "class" | "trait" | "interface" | "const"
  | "async" | "abstract" | "final" ->
      error env
          "Parse error: declarations are not supported outside global scope";
      ignore (ignore_toplevel ~attr:[] [] env (fun _ -> true));
      Noop
  | x ->
      L.back env.lb;
      let e = expr env in
      expect env Tsc;
      Expr e

(*****************************************************************************)
(* Break statement *)
(*****************************************************************************)

and statement_break env =
  let stmt = Break (Pos.make env.file env.lb) in
  check_continue env;
  stmt

(*****************************************************************************)
(* Continue statement *)
(*****************************************************************************)

and statement_continue env =
  let stmt = Continue (Pos.make env.file env.lb) in
  check_continue env;
  stmt

and check_continue env =
  match L.token env.file env.lb with
  | Tsc -> ()
  | Tint -> error_continue env
  | _ -> error_expect env ";"

(*****************************************************************************)
(* Throw statement *)
(*****************************************************************************)

and statement_throw env =
  let e = expr env in
  expect env Tsc;
  Throw e

(*****************************************************************************)
(* Return statement *)
(*****************************************************************************)

and statement_return env =
  let pos = Pos.make env.file env.lb in
  let value = return_value env in
  Return (pos, value)

and return_value env =
  match L.token env.file env.lb with
  | Tsc -> None
  | _ ->
      L.back env.lb;
      let e = expr env in
      expect env Tsc;
      Some e

(*****************************************************************************)
(* Static variables *)
(*****************************************************************************)

and statement_static env =
  let pos = Pos.make env.file env.lb in
  match L.token env.file env.lb with
  | Tlvar ->
      L.back env.lb;
      let el = static_var_list env in
      Static_var el
  | _ ->
      L.back env.lb;
      let id = pos, Id (pos, "static") in
      let e = expr_remain env id in
      Expr e

and static_var_list env =
  let error_state = !(env.errors) in
  let cst = static_var env in
  match L.token env.file env.lb with
  | Tsc ->
      [cst]
  | Tcomma ->
      if !(env.errors) != error_state
      then [cst]
      else cst :: static_var_list_remain env
  | _ -> error_expect env ";"; [cst]

and static_var_list_remain env =
  match L.token env.file env.lb with
  | Tsc -> []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let cst = static_var env in
      match L.token env.file env.lb with
      | Tsc ->
          [cst]
      | Tcomma ->
          if !(env.errors) != error_state
          then [cst]
          else cst :: static_var_list_remain env
      | _ ->
          error_expect env ";"; [cst]

and static_var env =
  expr env

(*****************************************************************************)
(* Switch statement *)
(*****************************************************************************)

and statement_switch env =
  let e = paren_expr env in
  expect env Tlcb;
  let casel = switch_body env in
  Switch (e, casel)

(* switch(...) { _ } *)
and switch_body env =
  match L.token env.file env.lb with
  | Trcb ->
      []
  | Tword ->
      let word = Lexing.lexeme env.lb in
      switch_body_word env word
  | _ ->
      error_expect env "}";
      []

and switch_body_word env = function
  | "case" ->
      let e = expr env in
      expect env Tcolon;
      let stl = case_body env in
      Case (e, stl) :: switch_body env
  | "default" ->
      expect env Tcolon;
      let stl = case_body env in
      Default stl :: switch_body env
  | _ -> error_expect env "case"; []

(* switch(...) { case/default: _ } *)
and case_body env =
  match L.token env.file env.lb with
  | Tword ->
      (match Lexing.lexeme env.lb with
      | "case" | "default" -> L.back env.lb; []
      | _ ->
          L.back env.lb;
          let error_state = !(env.errors) in
          let st = statement env in
          if !(env.errors) != error_state
          then [st]
          else st :: case_body env
      )
  | Trcb ->
      L.back env.lb; []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let st = statement env in
      if !(env.errors) != error_state
      then [st]
      else st :: case_body env

(*****************************************************************************)
(* If statement *)
(*****************************************************************************)

and statement_if env =
  let e = paren_expr env in
  let st1 = statement env in
  let st2 = statement_else env in
  If (e, [st1], [st2])

and statement_else env =
  match L.token env.file env.lb with
  | Tword ->
      (match Lexing.lexeme env.lb with
      | "else" -> statement env
      | "elseif" -> statement_if env
      | _ -> L.back env.lb; Noop
      )
  | _ -> L.back env.lb; Noop

(*****************************************************************************)
(* Do/While do statement *)
(*****************************************************************************)

and statement_do env =
  let st = statement env in
  expect_word env "while";
  let e = paren_expr env in
  expect env Tsc;
  Do ([st], e)

and statement_while env =
  let e = paren_expr env in
  let st = statement env in
  While (e, [st])

(*****************************************************************************)
(* For statement *)
(*****************************************************************************)

and statement_for env =
  expect env Tlp;
  let start = Pos.make env.file env.lb in
  let _ = L.token env.file env.lb in
  let _ = L.back env.lb in
  let last, el = for_expr env in
  let e1 = Pos.btw start last, Expr_list el in
  let start = last in
  let last, el = for_expr env in
  let e2 = Pos.btw start last, Expr_list el in
  let start = last in
  let last, el = for_last_expr env in
  let e3 = Pos.btw start last, Expr_list el in
  let st = statement env in
  For (e1, e2, e3, [st])

and for_expr env =
  match L.token env.file env.lb with
  | Tsc ->
      Pos.make env.file env.lb, []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let e = expr env in
      match L.token env.file env.lb with
      | Tsc ->
          Pos.make env.file env.lb, [e]
      | _ when !(env.errors) != error_state ->
          L.back env.lb;
          Pos.make env.file env.lb, [e]
      | Tcomma ->
            let last, el = for_expr env in
            last, e :: el
      | _ ->
          error_expect env ";";
          Pos.make env.file env.lb, [e]

and for_last_expr env =
  match L.token env.file env.lb with
  | Trp ->
      Pos.make env.file env.lb, []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let e = expr env in
      match L.token env.file env.lb with
      | Trp ->
          Pos.make env.file env.lb, [e]
      | _ when !(env.errors) != error_state ->
          L.back env.lb;
          Pos.make env.file env.lb, [e]
      | Tcomma ->
          let last, el = for_last_expr env in
          last, e :: el
      | _ ->
          error_expect env ")";
          Pos.make env.file env.lb, [e]

(*****************************************************************************)
(* Foreach statement *)
(*****************************************************************************)

and statement_foreach env =
  expect env Tlp;
  let e = expr env in
  let await =
    match L.token env.file env.lb with
    | Tword when Lexing.lexeme env.lb = "await" -> Some (Pos.make env.file env.lb)
    | _ -> L.back env.lb; None in
  expect_word env "as";
  let as_expr = foreach_as env in
  let st = statement env in
  Foreach (e, await, as_expr, [st])

and foreach_as env =
  let e1 = expr env in
  match L.token env.file env.lb with
  | Tsarrow ->
      let e2 = expr env in
      check_foreach_lvalue env e2;
      expect env Trp;
      As_kv (e1, e2)
  | Trp ->
      check_foreach_lvalue env e1;
      As_v e1
  | _ ->
      error_expect env ")";
      As_v e1

(*****************************************************************************)
(* Try statement *)
(*****************************************************************************)

and statement_try env =
  let st = statement env in
  let cl = catch_list env in
  let fin = finally env in
  Try ([st], cl, fin)

and catch_list env =
  match L.token env.file env.lb with
  | Tword when Lexing.lexeme env.lb = "catch" ->
      expect env Tlp;
      let name = identifier env in
      let e = variable env in
      expect env Trp;
      let st = statement env in
      (name, e, [st]) :: catch_list env
  | _ -> L.back env.lb; []

and finally env =
  match L.token env.file env.lb with
  | Tword when Lexing.lexeme env.lb = "finally" ->
    let st = statement env in
    [st]
  | _ -> L.back env.lb; []

(*****************************************************************************)
(* Echo statement *)
(*****************************************************************************)

and statement_echo env =
  let pos = Pos.make env.file env.lb in
  let args = echo_args env in
  let f = pos, Id (pos, "echo") in
  Expr (pos, Call (f, args, []))

and echo_args env =
  let e = expr env in
  match L.token env.file env.lb with
  | Tsc ->
      [e]
  | Tcomma ->
      e :: echo_args env
  | _ ->
      error_expect env ";"; []

(*****************************************************************************)
(* Function/Method parameters *)
(*****************************************************************************)

and parameter_list env =
  expect env Tlp;
  parameter_list_remain env

and parameter_list_remain env =
  match L.token env.file env.lb with
  | Trp -> []
  | Tellipsis ->
      [parameter_varargs env]
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let p = param ~variadic:false env in
      match L.token env.file env.lb with
      | Trp ->
          [p]
      | Tellipsis ->
          [p ; parameter_varargs env]
      | Tcomma ->
          if !(env.errors) != error_state
          then [p]
          else p :: parameter_list_remain env
      | _ ->
          error_expect env ")"; [p]

and parameter_varargs env =
  let pos = Pos.make env.file env.lb in
  (match L.token env.file env.lb with
    | Tcomma -> expect env Trp; make_param_ellipsis pos
    | Trp -> make_param_ellipsis pos;
    | _ ->
      L.back env.lb;
      let p = param ~variadic:true env in
      expect env Trp; p
  )

and make_param_ellipsis pos =
  { param_hint = None;
    param_is_reference = false;
    param_is_variadic = true;
    param_id = (pos, "...");
    param_expr = None;
    param_modifier = None;
    param_user_attributes = [];
  }

and param ~variadic env =
  let attrs = attribute env in
  let modifs = parameter_modifier env in
  let h = parameter_hint env in
  let is_ref, variadic_after_hint, name = ref_param env in
  assert ((not variadic_after_hint) || (not variadic));
  let variadic = variadic || variadic_after_hint in
  let default = parameter_default env in
  let default =
    if variadic && default <> None then
      let () = error env "Variadic arguments don't have default values" in
      None
    else default in
  if variadic_after_hint then begin
    expect env Trp;
    L.back env.lb
  end else ();
  { param_hint = h;
    param_is_reference = is_ref;
    param_is_variadic = variadic;
    param_id = name;
    param_expr = default;
    param_modifier = modifs;
    param_user_attributes = attrs;
  }

and parameter_modifier env =
  match L.token env.file env.lb with
  | Tword ->
      (match Lexing.lexeme env.lb with
      | "private" -> Some Private
      | "public" -> Some Public
      | "protected" -> Some Protected
      | _ -> L.back env.lb; None
      )
  | _ -> L.back env.lb; None

and parameter_hint env =
  if parameter_has_hint env
  then Some (hint env)
  else None

and parameter_has_hint env =
  look_ahead env begin fun env ->
    match L.token env.file env.lb with
    | Tellipsis | Tamp | Tlvar -> false
    | _ -> true
  end

and parameter_default env =
  match L.token env.file env.lb with
  | Teq ->
      let default = expr env in
      Some default
  | _ -> L.back env.lb; None

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

and expr env =
  let e1 = expr_atomic ~allow_class:false ~class_const:false env in
  let e2 = expr_remain env e1 in
  e2

and expr_list env =
  expect env Tlp;
  expr_list_remain env

and expr_list_remain env =
  match L.token env.file env.lb with
  | Trp -> []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let e = expr { env with priority = 0 } in
      match L.token env.file env.lb with
      | Trp ->
          [e]
      | Tcomma ->
          if !(env.errors) != error_state
          then [e]
          else e :: expr_list_remain env
      | _ -> error_expect env ")"; [e]

and expr_remain env e1 =
  match L.token env.file env.lb with
  | Tplus ->
      expr_binop env Tplus Plus e1
  | Tminus ->
      expr_binop env Tminus Minus e1
  | Tstar ->
      expr_binop env Tstar Star e1
  | Tstarstar ->
      expr_binop env Tstarstar Starstar e1
  | Tslash ->
      expr_binop env Tslash Slash e1
  | Teq ->
      expr_assign env Teq (Eq None) e1
  | Tbareq ->
      expr_assign env Tbareq (Eq (Some Bar)) e1
  | Tpluseq ->
      expr_assign env Tpluseq (Eq (Some Plus)) e1
  | Tstareq ->
      expr_assign env Tstareq (Eq (Some Star)) e1
  | Tslasheq ->
      expr_assign env Tslasheq (Eq (Some Slash)) e1
  | Tdoteq ->
      expr_assign env Tdoteq (Eq (Some Dot)) e1
  | Tminuseq ->
      expr_assign env Tminuseq (Eq (Some Minus)) e1
  | Tpercenteq ->
      expr_assign env Tpercenteq (Eq (Some Percent)) e1
  | Txoreq ->
      expr_assign env Txoreq (Eq (Some Xor)) e1
  | Tampeq ->
      expr_assign env Tampeq (Eq (Some Amp)) e1
  | Tlshifteq ->
      expr_assign env Tlshifteq (Eq (Some Ltlt)) e1
  | Trshifteq ->
      expr_assign env Trshifteq (Eq (Some Gtgt)) e1
  | Teqeqeq ->
      expr_binop env Teqeqeq EQeqeq e1
  | Tgt ->
      expr_binop env Tgt Gt e1
  | Tpercent ->
      expr_binop env Tpercent Percent e1
  | Tdot ->
      expr_binop env Tdot Dot e1
  | Teqeq ->
      expr_binop env Teqeq Eqeq e1
  | Tampamp ->
      expr_binop env Tampamp AMpamp e1
  | Tbarbar ->
      expr_binop env Tbarbar BArbar e1
  | Tdiff ->
      expr_binop env Tdiff Diff e1
  | Tlt ->
      expr_binop env Tlt Lt e1
  | Tdiff2 ->
      expr_binop env Tdiff2 Diff2 e1
  | Tgte ->
      expr_binop env Tgte Gte e1
  | Tlte ->
      expr_binop env Tlte Lte e1
  | Tamp ->
      expr_binop env Tamp Amp e1
  | Tbar ->
      expr_binop env Tbar Bar e1
  | Tltlt ->
      expr_binop env Tltlt Ltlt e1
  | Tgtgt ->
      expr_binop env Tgtgt Gtgt e1
  | Txor ->
      expr_binop env Txor Xor e1
  | Tincr | Tdecr as uop  ->
      expr_postfix_unary env uop e1
  | Tarrow | Tnsarrow as tok ->
      expr_arrow env e1 tok
  | Tcolcol ->
      expr_colcol env e1
  | Tlp ->
      expr_call env e1
  | Tlb ->
      expr_array_get env e1
  | Tlcb ->
      error env "Do not use { to subscript, use [";
      expr_array_get env e1
  | Tqm ->
      expr_if env e1
  | Tword when Lexing.lexeme env.lb = "instanceof" ->
      expr_instanceof env e1
  | Tword when Lexing.lexeme env.lb = "and" ->
      error env ("Do not use \"and\", it has surprising precedence. "^
        "Use \"&&\" instead");
      expr_binop env Tampamp AMpamp e1
  | Tword when Lexing.lexeme env.lb = "or" ->
      error env ("Do not use \"or\", it has surprising precedence. "^
        "Use \"||\" instead");
      expr_binop env Tbarbar BArbar e1
  | Tword when Lexing.lexeme env.lb = "xor" ->
      error env ("Do not use \"xor\", it has surprising precedence. "^
        "Cast to bool and use \"^\" instead");
      expr_binop env Txor Xor e1
  | _ ->
      L.back env.lb; e1

(*****************************************************************************)
(* Expression reducer *)
(*****************************************************************************)

and reduce env e1 op make =
  let e, continue = reduce_ env e1 op make in
  if continue then expr_remain env e else e

and reduce_ env e1 op make =
  let current_prio = env.priority in
  let assoc, prio = get_priority op in
  let env = { env with priority = prio } in
  if prio = current_prio
  then
    match assoc with
    | Left ->
        let e = make e1 { env with priority = env.priority + 1 } in
        expr_remain env e, true
    | Right ->
        let e = make e1 env in
        e, false
    | NonAssoc ->
        error env "This operator is not associative, add parentheses";
        let e = make e1 env in
        e, false
  else if prio < current_prio
  then begin
    L.back env.lb;
    e1, false
  end
  else begin
    assert (prio > current_prio);
    if assoc = NonAssoc
    then make e1 env, true
    else reduce_ env e1 op make
  end

(*****************************************************************************)
(* lambda expressions *)
(*****************************************************************************)

and lambda_expr_body : env -> block = fun env ->
  (* check for an async block *)
  let tok = L.token env.file env.lb in
  let value = Lexing.lexeme env.lb in
  let () =
    if tok <> Tword || value <> "async"
    then L.back env.lb
    else
      (* in sync lambda: just transform into an async lambda *)
      (* in async lambda: be explicit about the Awaitable<Awaitable<X>> return
       * type with a return statement *)
      error_back env "Unexpected use of async {...} as lambda expression"
  in

  let (p, e1) = expr env in
  [Return (p, (Some (p, e1)))]

and lambda_body ~sync env params ret =
  let is_generator, body_stmts =
    (if peek env = Tlcb
     then function_body env
     (* as of Apr 2015, a lambda expression body can't contain a yield *)
     else false, (lambda_expr_body env))
  in
  let f_fun_kind = fun_kind sync is_generator in
  let f = {
    f_name = (Pos.none, ";anonymous");
    f_tparams = [];
    f_params = params;
    f_ret = ret;
    f_ret_by_ref = false;
    f_body = body_stmts;
    f_user_attributes = [];
    f_fun_kind;
    f_mode = env.mode;
    f_mtime = 0.0;
    f_namespace = Namespace_env.empty;
  }
  in Lfun f

and make_lambda_param : id -> fun_param = fun var_id ->
  {
    param_hint = None;
    param_is_reference = false;
    param_is_variadic = false;
    param_id = var_id;
    param_expr = None;
    param_modifier = None;
    param_user_attributes = [];
  }

and lambda_single_arg ~(sync:fun_decl_kind) env var_id : expr_ =
  expect env Tlambda;
  lambda_body ~sync env [make_lambda_param var_id] None

and try_short_lambda env =
  try_parse env begin fun env ->
    let error_state = !(env.errors) in
    let param_list = parameter_list_remain env in
    if !(env.errors) != error_state then begin
      env.errors := error_state;
      None
    end else begin
      let ret = hint_return_opt env in
      if !(env.errors) != error_state then begin
        env.errors := error_state;
        None
      end else if not (peek env = Tlambda)
      then None
      else begin
        drop env;
        Some (lambda_body ~sync:FDeclSync env param_list ret)
      end
    end
  end

and try_xhp_attr_use env =
  try_parse env begin fun env ->
    match L.token env.file env.lb with
      | Tcolon ->
        (match L.xhpname env.file env.lb with
        | Txhpname ->
          let name = (Pos.make env.file env.lb, ":"^Lexing.lexeme env.lb) in
            (match L.token env.file env.lb with
            | Tcomma | Tsc ->
              L.back env.lb;
              Some (XhpAttrUse (class_hint_with_name env name))
            | _ ->
              L.back env.lb;
              None)
        | _ -> None)
      | _ -> None
  end

and try_xhp_enum_hint env =
  try_parse env begin fun env ->
    match L.token env.file env.lb with
      | Tword when Lexing.lexeme env.lb = "enum" ->
        let pos = Pos.make env.file env.lb in
        expect env Tlcb;
        let items = xhp_enum_decl_list env in
        Some (pos, items)
      | _ -> None
  end

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

and expr_atomic ~allow_class ~class_const env =
  let tok = L.token env.file env.lb in
  let pos = Pos.make env.file env.lb in
  match tok with
  | Tint ->
      let tok_value = Lexing.lexeme env.lb in
      pos, Int (pos, tok_value)
  | Tfloat ->
      let tok_value = Lexing.lexeme env.lb in
      pos, Float (pos, tok_value)
  | Tquote ->
      let absolute_pos = env.lb.Lexing.lex_curr_pos in
      expr_string env pos absolute_pos
  | Tdquote ->
      expr_encapsed env pos
  | Tlvar ->
      let tok_value = Lexing.lexeme env.lb in
      let var_id = (pos, tok_value) in
      pos, if peek env = Tlambda
        then lambda_single_arg ~sync:FDeclSync env var_id
        else Lvar var_id
  | Tcolon ->
      L.back env.lb;
      let name = identifier env in
      fst name, Id name
  | Tem | Tincr | Tdecr | Ttild | Tplus | Tminus as op ->
      expr_prefix_unary env pos op
  | Tamp ->
      expr_ref env pos
  | Tat ->
      with_priority env Tat expr
  | Tword ->
      let word = Lexing.lexeme env.lb in
      expr_atomic_word ~allow_class ~class_const env pos word
  | Tlp ->
      (match try_short_lambda env with
      | None ->
          if is_cast env
          then expr_cast env pos
          else with_base_priority env begin fun env ->
            let e = expr env in
            expect env Trp;
            let end_ = Pos.make env.file env.lb in
            Pos.btw pos end_, snd e
          end
      | Some l -> pos, l
      )
  | Tlb ->
      expr_short_array env pos
  | Tlt when is_xhp env ->
      xhp env
  | Theredoc ->
      expr_heredoc env
  | Tdollar ->
      error env ("A valid variable name starts with a letter or underscore,"^
        "followed by any number of letters, numbers, or underscores");
      expr env
  | Tunsafeexpr ->
      let e = expr env in
      let end_ = Pos.make env.file env.lb in
      Pos.btw pos end_, Unsafeexpr e
  | _ ->
      error_expect env "expression";
      pos, Null

and expr_atomic_word ~allow_class ~class_const env pos = function
  | "class" when not allow_class ->
      error_expect env "expression";
      pos, Null
  | "final" | "abstract" | "interface" | "trait" ->
      error_expect env "expression";
      pos, Null
  | "true" when not class_const ->
      pos, True
  | "false" when not class_const ->
      pos, False
  | "null" when not class_const ->
      pos, Null
  | "array" ->
      expr_array env pos
  | "shape" ->
      expr_shape env pos
  | "new" ->
      expr_new env pos
  | "async" ->
      expr_anon_async env pos
  | "function" ->
      expr_anon_fun env pos ~sync:FDeclSync
  | name when is_collection env ->
      expr_collection env pos name
  | "await" ->
      expr_await env pos
  | "yield" ->
      env.in_generator := true;
      expr_yield env pos
  | "clone" ->
      expr_clone env pos
  | "list" ->
      expr_php_list env pos
  | r when is_import r ->
      if env.mode = FileInfo.Mstrict
      then
        error env
          ("Parse error: "^r^" is supported only as a toplevel "^
          "declaration");
      expr_import r env pos
  | x when not class_const && String.lowercase x = "true" ->
      Lint.lowercase_constant pos x;
      pos, True
  | x when not class_const && String.lowercase x = "false" ->
      Lint.lowercase_constant pos x;
      pos, False
  | x when not class_const && String.lowercase x = "null" ->
      Lint.lowercase_constant pos x;
      pos, Null
  | x when String.lowercase x = "array" ->
      expr_array env pos
  | x ->
      pos, Id (pos, x)

(*****************************************************************************)
(* Expressions in parens. *)
(*****************************************************************************)

and paren_expr env =
  with_base_priority env begin fun env ->
    expect env Tlp;
    let e = expr env in
    expect env Trp;
    e
  end

(*****************************************************************************)
(* Assignments (=, +=, -=, ...) *)
(*****************************************************************************)

and expr_assign env bop ast_bop e1 =
  reduce env e1 bop begin fun e1 env ->
    check_lvalue env e1;
    let e2 = expr { env with priority = 0 } in
    btw e1 e2, Binop (ast_bop, e1, e2)
  end

(*****************************************************************************)
(* Binary operations (+, -, /, ...) *)
(*****************************************************************************)

and expr_binop env bop ast_bop e1 =
  reduce env e1 bop begin fun e1 env ->
    let e2 = expr env in
    btw e1 e2, Binop (ast_bop, e1, e2)
  end

(*****************************************************************************)
(* Object Access ($obj->method) *)
(*****************************************************************************)

and expr_arrow env e1 tok =
  reduce env e1 tok begin fun e1 env ->
    let e2 =
      match L.varname env.lb with
      | Tword ->
          let name = Lexing.lexeme env.lb in
          let pos = Pos.make env.file env.lb in
          pos, Id (pos, name)
      | _ -> L.back env.lb; expr env
    in
    btw e1 e2, (match tok with
      | Tarrow -> Obj_get (e1, e2, OG_nullthrows)
      | Tnsarrow -> Obj_get (e1, e2, OG_nullsafe)
      | _ -> assert false)
  end

(*****************************************************************************)
(* Class Access (ClassName::method_name) *)
(*****************************************************************************)

and expr_colcol env e1 =
  reduce env e1 Tcolcol begin fun e1 env ->
    (match e1 with
    | (_, Id cname) ->
        (* XYZ::class is OK ... *)
        expr_colcol_remain ~allow_class:true env e1 cname
    | pos, Lvar cname  ->
        (* ... but get_class($x) should be used instead of $x::class *)
        expr_colcol_remain ~allow_class:false env e1 cname
    | pos, _ ->
        error_at env pos "Expected class name";
        e1
    )
  end

and expr_colcol_remain ~allow_class env e1 cname =
  match expr_atomic env ~allow_class ~class_const:true with
  | _, Lvar x ->
      btw e1 x, Class_get (cname, x)
  | _, Id x ->
      btw e1 x, Class_const (cname, x)
  | pos, _ ->
      error_at env pos "Expected identifier";
      e1

(*****************************************************************************)
(* Function call (f(params)) *)
(*****************************************************************************)

and expr_call env e1 =
  reduce env e1 Tlp begin fun e1 env ->
    L.back env.lb;
    let args1, args2 = expr_call_list env in
    let end_ = Pos.make env.file env.lb in
    Pos.btw (fst e1) end_, Call (e1, args1, args2)
  end

(* An expr_call_list is the same as an expr_list except for the possibility
 * of ParamUnpack (aka splat) calls of the form:
 *   f(...$unpacked);
 *)
and expr_call_list env =
  expect env Tlp;
  expr_call_list_remain env

and expr_call_list_remain env =
  match L.token env.file env.lb with
    | Trp -> [], []
    | Tellipsis -> (* f($x, $y, << ...$args >> ) *)
      let unpack_e = expr { env with priority = 0 } in
      (* no regular params after an unpack *)
      (match L.token env.file env.lb with
        | Tcomma ->
            expect env Trp;
            [], [unpack_e]
        | Trp -> [], [unpack_e]
        | _ -> error_expect env ")"; [], [unpack_e])
    | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let e = expr { env with priority = 0 } in
      match L.token env.file env.lb with
        | Trp -> [e], []
        | Tcomma ->
          if !(env.errors) != error_state
          then [e], []
          else begin
            let reg, unpack = expr_call_list_remain env
            in e :: reg, unpack
          end
        | _ -> error_expect env ")"; [e], []

(*****************************************************************************)
(* Collections *)
(*****************************************************************************)

and is_collection env = peek env = Tlcb

and expr_collection env pos name =
  if is_collection env
  then build_collection env pos name
  else pos, Id (pos, name)

and build_collection env pos name =
  let name = pos, name in
  let fds = collection_field_list env in
  let end_ = Pos.make env.file env.lb in
  Pos.btw pos end_, Collection (name, fds)

and collection_field_list env =
  expect env Tlcb;
  collection_field_list_remain env

and collection_field_list_remain env =
  match L.token env.file env.lb with
  | Trcb -> []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let fd = array_field env in
      match L.token env.file env.lb with
      | Trcb ->
          [fd]
      | Tcomma ->
          if !(env.errors) != error_state
          then [fd]
          else fd :: collection_field_list_remain env
      | _ ->
          error_expect env "}"; []

(*****************************************************************************)
(* Imports - require/include/require_once/include_once *)
(*****************************************************************************)

and is_import r =
  List.mem r ["require"; "require_once"; "include"; "include_once"]

and expr_import r env start =
  let flavor = match r with
  | "require" -> Require
  | "include" -> Include
  | "require_once" -> RequireOnce
  | "include_once" -> IncludeOnce
    (* We just checked for this very condition *)
  | _ -> assert false in
  (* all the import statements have the same priority *)
  with_priority env Timport begin fun env ->
    let e = expr env in
    Pos.btw start (fst e), Import (flavor, e)
  end

(*****************************************************************************)
(* InstanceOf *)
(*****************************************************************************)

and expr_instanceof env e1 =
  reduce env e1 Tinstanceof begin fun e1 env ->
    let e2 = expr env in
    btw e1 e2, InstanceOf (e1, e2)
  end

(*****************************************************************************)
(* Yield/Await *)
(*****************************************************************************)

and expr_yield env start =
  with_priority env Tyield begin fun env ->
    match L.token env.file env.lb with
    | Tword when Lexing.lexeme env.lb = "break" ->
        let end_ = Pos.make env.file env.lb in
        Pos.btw start end_, Yield_break
    | _ ->
        L.back env.lb;
        let af = array_field env in
        start, Yield af
  end

and expr_await env start =
  with_priority env Tawait begin fun env ->
    let e = expr env in
    Pos.btw start (fst e), Await e
  end

(*****************************************************************************)
(* Clone *)
(*****************************************************************************)

and expr_clone env start =
  with_base_priority env begin fun env ->
    let e = expr env in
    Pos.btw start (fst e), Clone e
  end

(*****************************************************************************)
(* List *)
(*****************************************************************************)

and expr_php_list env start =
  let el = expr_list env in
  let end_ = Pos.make env.file env.lb in
  Pos.btw start end_, List el

(*****************************************************************************)
(* Anonymous functions *)
(*****************************************************************************)

and expr_anon_async env pos =
  match L.token env.file env.lb with
  | Tword when Lexing.lexeme env.lb = "function" ->
      expr_anon_fun env pos ~sync:FDeclAsync
  | Tlvar ->
      let var_pos = Pos.make env.file env.lb in
      pos, lambda_single_arg ~sync:FDeclAsync env (var_pos, Lexing.lexeme env.lb)
  | Tlp ->
      let param_list = parameter_list_remain env in
      let ret = hint_return_opt env in
      expect env Tlambda;
      pos, lambda_body ~sync:FDeclAsync env param_list ret
  | Tlcb -> (* async { ... } *)
      L.back env.lb;
      let lambda = pos, lambda_body ~sync:FDeclAsync env [] None in
      pos, Call (lambda, [], [])
  | _ ->
      L.back env.lb;
      pos, Id (pos, "async")

and expr_anon_fun env pos ~(sync:fun_decl_kind) =
  let env = { env with priority = 0 } in
  let params = parameter_list env in
  let ret = hint_return_opt env in
  let use = function_use env in
  let is_generator, body_stmts = function_body env in
  let f = {
    f_name = (Pos.none, ";anonymous");
    f_tparams = [];
    f_params = params;
    f_ret = ret;
    f_ret_by_ref = false;
    f_body = body_stmts;
    f_user_attributes = [];
    f_fun_kind = fun_kind sync is_generator;
    f_mode = env.mode;
    f_mtime = 0.0;
    f_namespace = Namespace_env.empty;
  }
  in
  pos, Efun (f, use)

(*****************************************************************************)
(* Use (for functions) *)
(*****************************************************************************)

and function_use env =
  match L.token env.file env.lb with
  | Tword when Lexing.lexeme env.lb = "use" ->
      expect env Tlp;
      use_list env
  | _ -> L.back env.lb; []

and use_list env =
  match L.token env.file env.lb with
  | Trp -> []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let var = ref_variable env in
      match L.token env.file env.lb with
      | Tcomma ->
          if !(env.errors) != error_state
          then [var]
          else var :: use_list env
      | Trp ->
          [var]
      | _ ->
          error_expect env ")";
          [var]

(*****************************************************************************)
(* New: new ClassName(...) *)
(*****************************************************************************)

and expr_new env pos_start =
  with_priority env Tnew begin fun env ->
    let cname =
      let e = expr env in
      match e with
      | _, Lvar _
      | _, Array_get _
      | _, Obj_get _
      | _, Class_get _
      | _, Call _
      | _, Id _ -> e
      | p, _ ->
          error_expect env "class name";
          e
    in
    let args1, args2 = expr_call_list env in
    let pos_end = Pos.make env.file env.lb in
    Pos.btw pos_start pos_end, New (cname, args1, args2)
  end

(*****************************************************************************)
(* Casts: (int|..|float) expr *)
(*****************************************************************************)

and is_cast_type = function
    | "int" | "float" | "double" | "string"
    | "array" | "object" | "bool" | "unset" -> true
    | _ -> false

(* (int), (float), etc are considered cast tokens by HHVM, so we will always
 * interpret them as casts. I.e. (object) >> 1 is a parse error because it is
 * trying to cast the malformed expression `>> 1` to an object. On the other
 * hand, (x) >> 1 is parsed like `x >> 1`, because (x) is not a cast token. *)
and is_cast env =
  look_ahead env begin fun env ->
    L.token env.file env.lb = Tword &&
    let cast_name = Lexing.lexeme env.lb in
    L.token env.file env.lb = Trp && begin
      is_cast_type cast_name ||
      match L.token env.file env.lb with
      (* We cannot be making a cast if the next token is a binary / ternary
       * operator, or if it's the end of a statement (i.e. a semicolon.) *)
      | Tqm | Tsc | Tstar | Tslash | Txor | Tpercent | Tlt | Tgt | Tltlt | Tgtgt
      | Tlb | Trb | Tdot | Tlambda -> false
      | _ -> true
    end
  end

and expr_cast env start_pos =
  with_priority env Tcast begin fun env ->
    let tok = L.token env.file env.lb in
    let cast_type = Lexing.lexeme env.lb in
    assert (tok = Tword);
    let p = Pos.make env.file env.lb in
    expect env Trp;
    let ty = p, Happly ((p, cast_type), []) in
    let e = expr env in
    Pos.btw start_pos (fst e), Cast (ty, e)
  end

(*****************************************************************************)
(* Unary operators $i++ etc ... *)
(*****************************************************************************)

and unary_priority = function
  | Tplus | Tminus -> Tincr
  | x -> x

and expr_prefix_unary env start op =
  with_priority env (unary_priority op) begin fun env ->
    let e = expr env in
    let op =
      match op with
      | Tem -> Unot
      | Tincr -> (check_lvalue env e; Uincr)
      | Tdecr -> (check_lvalue env e; Udecr)
      | Ttild -> Utild
      | Tplus -> Uplus
      | Tminus -> Uminus
      | _ -> assert false
    in
    Pos.btw start (fst e), Unop (op, e)
  end

and expr_postfix_unary env uop e1 =
  let end_ = Pos.make env.file env.lb in
  let op =
    check_lvalue env e1;
    match uop with
    | Tincr -> Upincr
    | Tdecr -> Updecr
    | _ -> assert false
  in
  let e = Pos.btw (fst e1) end_, Unop (op, e1) in
  expr_remain env e

(*****************************************************************************)
(* If expression: _?_:_ *)
(*****************************************************************************)

and is_colon_if env =
  look_ahead env begin fun env ->
    let tok = L.token env.file env.lb in
    tok = Tcolon &&
    (* At this point, we might still be dealing with an xhp identifier *)
    L.no_space_id env.lb <> Tword
  end

and expr_if env e1 =
  reduce env e1 Tqm begin fun e1 env ->
    if is_colon_if env
    then colon_if env e1
    else ternary_if env e1
  end

and ternary_if env e1 =
  let e2 = expr { env with priority = 0 } in
  expect env Tcolon;
  let e3 = expr env in
  (match e1 with
  | pos, Eif _ ->
      error_at env pos "You should add parentheses"
  | _ -> ());
  Pos.btw (fst e1) (fst e3), Eif (e1, Some e2, e3)

and colon_if env e1 =
  expect env Tcolon;
  let e2 = expr env in
  Pos.btw (fst e1) (fst e2), Eif (e1, None, e2)


(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

and expr_string env start abs_start =
  match L.string env.file env.lb with
  | Tquote ->
      let pos = Pos.btw start (Pos.make env.file env.lb) in
      let len = env.lb.Lexing.lex_curr_pos - abs_start - 1 in
      let content = String.sub env.lb.Lexing.lex_buffer abs_start len in
      pos, String (pos, content)
  | Teof ->
      error_at env start "string not closed";
      start, String (start, "")
  | _ -> assert false

and expr_encapsed env start =
  let abs_start = env.lb.Lexing.lex_curr_pos in
  let pos_start = Pos.make env.file env.lb in
  let el = encapsed_nested pos_start env in
  let pos_end = Pos.make env.file env.lb in
  let pos = Pos.btw pos_start pos_end in
  let len = env.lb.Lexing.lex_curr_pos - abs_start - 1 in
  let content = String.sub env.lb.Lexing.lex_buffer abs_start len in
  pos, String2 (el, (pos, content))

and encapsed_nested start env =
  match L.string2 env.file env.lb with
  | Tdquote ->
      []
  | Teof ->
      error_at env start "string not properly closed";
      []
  | Tlcb when env.mode = FileInfo.Mdecl ->
      encapsed_nested start env
  | Tlcb ->
      (match L.string2 env.file env.lb with
      | Tdollar ->
          error env "{ not supported";
          L.back env.lb;
          encapsed_nested start env
      | Tlvar ->
          L.back env.lb;
          let error_state = !(env.errors) in
          let e = expr env in
          (match L.string2 env.file env.lb with
          | Trcb -> ()
          | _ -> error_expect env "}");
          if !(env.errors) != error_state
          then [e]
          else e :: encapsed_nested start env
      | _ ->
          L.back env.lb;
          encapsed_nested start env
      )
  | Trcb ->
      encapsed_nested start env
  | Tdollar ->
      (match L.string2 env.file env.lb with
      | Tlcb ->
          if env.mode = FileInfo.Mstrict
          then error env "${ not supported";
          let error_state = !(env.errors) in
          let result = (match L.string2 env.file env.lb with
          | Tword ->
              (* The first token after ${ will lex as a word, but is actually
               * an lvar, so we need to fix it up. For example, "${foo}" should
               * be Lvar $foo, but will lex as Tdollar-Tlcb-Tword foo. *)
              let pos = Pos.make env.file env.lb in
              let lvar = pos, Lvar (pos, "$" ^ Lexing.lexeme env.lb) in
              encapsed_expr_reduce pos env lvar
          | _ ->
              error_expect env "variable";
              Pos.make env.file env.lb, Null) in
          expect env Trcb;
          if !(env.errors) != error_state
          then [result]
          else result :: encapsed_nested start env
      | _ ->
          L.back env.lb;
          encapsed_nested start env
      )
  | Tlvar ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let e = encapsed_expr env in
      if !(env.errors) != error_state
      then [e]
      else e :: encapsed_nested start env
  | _ -> encapsed_nested start env

and encapsed_expr env =
  match L.string2 env.file env.lb with
  | Tlcb when env.mode = FileInfo.Mdecl ->
      Pos.make env.file env.lb, Null
  | Tquote ->
      let pos = Pos.make env.file env.lb in
      let absolute_pos = env.lb.Lexing.lex_curr_pos in
      expr_string env pos absolute_pos
  | Tint ->
      let pos = Pos.make env.file env.lb in
      let tok_value = Lexing.lexeme env.lb in
      pos, Int (pos, tok_value)
  | Tword ->
      let pid = Pos.make env.file env.lb in
      let id = Lexing.lexeme env.lb in
      pid, (Id (pid, id))
  | Tlvar ->
      let pos = Pos.make env.file env.lb in
      let lvar = pos, Lvar (pos, Lexing.lexeme env.lb) in
      encapsed_expr_reduce pos env lvar
  | _ ->
      error_expect env "expression";
      Pos.make env.file env.lb, Null

and encapsed_expr_reduce start env e1 =
  let e1, continue = encapsed_expr_reduce_left start env e1 in
  if continue
  then encapsed_expr_reduce start env e1
  else e1

and encapsed_expr_reduce_left start env e1 =
  match L.string2 env.file env.lb with
  | Tlb ->
      let e2 =
        match L.string2 env.file env.lb with
        | Tword ->
            (* We need to special case this because any identifier
             * (including keywords) is allowed in this context.
             * For example: $x[function] is legal.
             *)
            let pid = Pos.make env.file env.lb in
            let id = Lexing.lexeme env.lb in
            pid, (String (pid, id))
        | _ ->
            L.back env.lb;
            expr { env with priority = 0 }
      in
      (match L.string2 env.file env.lb with
      | Trb -> ()
      | _ -> error_expect env "]"
      );
      let pos = Pos.btw start (Pos.make env.file env.lb) in
      (pos, Array_get (e1, Some e2)), true
  | Tarrow ->
      (match L.string2 env.file env.lb with
      | Tword ->
          L.back env.lb;
          let e2 = encapsed_expr env in
          let pos = Pos.btw start (Pos.make env.file env.lb) in
          (pos, Obj_get (e1, e2, OG_nullthrows)), true
      | _ ->
          L.back env.lb;
          e1, false
      )
  | _ ->
      L.back env.lb;
      e1, false

(*****************************************************************************)
(* Heredocs *)
(*****************************************************************************)

and expr_heredoc env =
  let abs_start = env.lb.Lexing.lex_curr_pos in
  let tag = heredoc_tag env in
  heredoc_body tag env;
  let len = env.lb.Lexing.lex_curr_pos - abs_start - 1 in
  let content = String.sub env.lb.Lexing.lex_buffer abs_start len in
  fst tag, String (fst tag, content)

and heredoc_tag env =
  match L.token env.file env.lb with
  | Tword ->
      Pos.make env.file env.lb, Lexing.lexeme env.lb
  | Tquote ->
      let pos = Pos.make env.file env.lb in
      let abs_pos = env.lb.Lexing.lex_curr_pos in
      (match expr_string env pos abs_pos with
      | _, String x -> x
      | _ -> assert false)
  | _ ->
      error_expect env "heredoc or nowdoc identifier";
      Pos.make env.file env.lb, "HEREDOC"

and heredoc_body (pos, tag_value as tag) env =
  match L.heredoc_token env.lb with
  | Tnewline ->
      heredoc_end tag env
  | Teof ->
      error_expect env tag_value
  | _ ->
      heredoc_body tag env

and heredoc_end (pos, tag_value as tag) env =
  match L.heredoc_token env.lb with
  | Tword ->
      let tag2 = Lexing.lexeme env.lb in
      (match L.heredoc_token env.lb with
      | Tnewline when tag2 = tag_value ->
          ()
      | Tnewline ->
          heredoc_end tag env
      | Tsc when tag2 = tag_value ->
          L.back env.lb;
          ()
      | _ ->
          heredoc_body tag env
      )
  | Tnewline ->
      heredoc_end tag env
  | _ ->
      heredoc_body tag env


(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

and expr_array env pos =
  let fields = array_field_list env in
  pos, Array fields

and array_field_list env =
  expect env Tlp;
  array_field_list_remain env Trp []

and expr_short_array env pos =
  let fields = array_field_list_remain env Trb [] in
  pos, Array fields

and array_field_list_remain env terminal acc =
  match L.token env.file env.lb with
  | x when x = terminal -> List.rev acc
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let fd = array_field env in
      let acc = fd :: acc in
      match L.token env.file env.lb with
      | x when x = terminal ->
          List.rev acc
      | Tcomma ->
          if !(env.errors) != error_state
          then List.rev acc
          else array_field_list_remain env terminal acc
      | _ -> error_expect env ")"; [fd]

and array_field env =
  let env = { env with priority = 0 } in
  let e1 = expr env in
  match L.token env.file env.lb with
  | Tsarrow ->
      let e2 = expr env in
      AFkvalue (e1, e2)
  | _ ->
      L.back env.lb;
      AFvalue e1

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

and expr_shape env pos =
  let fields = shape_field_list env in
  pos, Shape fields

and shape_field_list env =
  expect env Tlp;
  shape_field_list_remain env

and shape_field_list_remain env =
  match L.token env.file env.lb with
  | Trp -> []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let fd = shape_field env in
      match L.token env.file env.lb with
      | Trp ->
          [fd]
      | Tcomma ->
          if !(env.errors) != error_state
          then [fd]
          else fd :: shape_field_list_remain env
      | _ -> error_expect env ")"; [fd]

and shape_field env =
  let name = shape_field_name env in
  expect env Tsarrow;
  let value = expr { env with priority = 0 } in
  name, value

and shape_field_name env =
  let pos, e = expr env in
  match e with
  | String p -> SFlit p
  | Class_const (id, ps) -> SFclass_const (id, ps)
  | String2 (_, _) ->
     error env
           ("Shape field names cannot be strings enclosed by double quotes."
            ^" Use single quotes instead.");
     SFlit (pos, "")
  | _ -> error_expect env "string literal or class constant";
    SFlit (pos, "")


(*****************************************************************************)
(* Array access ($my_array[]|$my_array[_]) *)
(*****************************************************************************)

and expr_array_get env e1 =
  reduce env e1 Tlb begin fun e1 env ->
    match L.token env.file env.lb with
    | Trb ->
        let end_ = Pos.make env.file env.lb in
        Pos.btw (fst e1) end_, Array_get (e1, None)
    | _ ->
        L.back env.lb;
        let e2 = expr { env with priority = 0 } in
        expect env Trb;
        let end_ = Pos.make env.file env.lb in
        Pos.btw (fst e1) end_, Array_get (e1, Some e2)
  end

(*****************************************************************************)
(* Reference (&$v|&func()|&$obj->prop *)
(*****************************************************************************)

and expr_ref env start =
  with_priority env Tref begin fun env ->
    let e = expr env in
    Pos.btw start (fst e), Ref e
  end

(*****************************************************************************)
(* XHP *)
(*****************************************************************************)

and is_xhp env =
  look_ahead env begin fun env ->
    let tok = L.xhpname env.file env.lb in
    tok = Txhpname &&
    let tok2 = L.xhpattr env.file env.lb in
    tok2 = Tgt || tok2 = Tword ||
    (tok2 = Tslash && L.xhpattr env.file env.lb = Tgt)
  end

and xhp env =
  match L.xhpname env.file env.lb with
  | Txhpname ->
      let start = Pos.make env.file env.lb in
      let name = Lexing.lexeme env.lb in
      let pname = start, ":"^name in
      let attrl, closed = xhp_attributes env in
      let end_tag = Pos.make env.file env.lb in
      if closed
      then Pos.btw start end_tag, Xml (pname, attrl, [])
      else
        let tag_pos = Pos.btw start end_tag in
        let el = xhp_body tag_pos name env in
        let end_ = Pos.make env.file env.lb in
        Pos.btw start end_, Xml (pname, attrl, el)
  | _ ->
      error_expect env "xhpname";
      let pos = Pos.make env.file env.lb in
      pos, Xml ((pos, "xhp"), [], [])

and xhp_attributes env =
  match L.xhpattr env.file env.lb with
  | Tslash ->
      if L.xhpattr env.file env.lb <> Tgt
      then error_expect env ">";
      [], true
  | Tgt ->
      [], false
  | Tword ->
      let error_state = !(env.errors) in
      let attr_name = Pos.make env.file env.lb, Lexing.lexeme env.lb in
      expect env Teq;
      let attr_value = xhp_attribute_value env in
      if !(env.errors) != error_state
      then
        [attr_name, attr_value], true
      else
        let rl, closed = xhp_attributes env in
        (attr_name, attr_value) :: rl, closed
  | _ ->
      error_expect env ">";
      [], true

and xhp_attribute_value env =
  match L.xhpattr env.file env.lb with
  | Tlcb when env.mode = FileInfo.Mdecl ->
      ignore_body env;
      Pos.none, Null
  | Tlcb ->
      let result = expr { env with priority = 0 } in
      expect env Trcb;
      result
  | Tdquote ->
      let start = Pos.make env.file env.lb in
      let abs_start = env.lb.Lexing.lex_curr_pos in
      xhp_attribute_string env start abs_start
  | _ ->
      error_expect env "attribute value";
      let pos = Pos.make env.file env.lb in
      pos, String (pos, "")

and xhp_attribute_string env start abs_start =
  match L.string2 env.file env.lb with
  | Teof ->
      error_at env start "Xhp attribute not closed";
      start, String (start, "")
  | Tdquote ->
      let len = env.lb.Lexing.lex_curr_pos - abs_start - 1 in
      let content = String.sub env.lb.Lexing.lex_buffer abs_start len in
      let pos = Pos.btw start (Pos.make env.file env.lb) in
      pos, String (pos, content)
  | _ ->
      xhp_attribute_string env start abs_start

and xhp_body pos name env =
  match L.xhptoken env.file env.lb with
  | Tlcb when env.mode = FileInfo.Mdecl ->
      ignore_body env;
      xhp_body pos name env
  | Tlcb ->
      let error_state = !(env.errors) in
      let e = expr { env with priority = 0 } in
      expect env Trcb;
      if !(env.errors) != error_state
      then [e]
      else e :: xhp_body pos name env
  | Tlt ->
      if is_xhp env
      then
        (match xhp env with
        | (_, Xml (_, _, _)) as xml ->
            xml :: xhp_body pos name env
        | _ -> xhp_body pos name env)
      else
        (match L.xhptoken env.file env.lb with
        | Tslash ->
            let closing_tok = L.xhpname env.file env.lb in
            let closing_name = Lexing.lexeme env.lb in
            if closing_tok = Txhpname &&
              (L.xhptoken env.file env.lb = Tgt)
            then
              if closing_name = name
              then []
              else begin
                error_expect env name;
                []
              end
            else xhp_body pos name env
        | _ ->
            L.back env.lb;
            xhp_body pos name env
        )
  | Teof ->
      error_at env pos "Xhp tag not closed";
      []
  | Tword ->
      xhp_body pos name env
  | _ -> xhp_body pos name env

(*****************************************************************************)
(* Typedefs *)
(*****************************************************************************)

and typedef ~attr ~is_abstract env =
  let id = identifier env in
  let tparams = class_params env in
  let tconstraint = typedef_constraint env in
  expect env Teq;
  let td = hint env in
  expect env Tsc;
  let kind = if is_abstract then NewType td else Alias td in
  {
    t_id = id;
    t_tparams = tparams;
    t_constraint = tconstraint;
    t_kind = kind;
    t_user_attributes = attr;
    t_namespace = Namespace_env.empty;
    t_mode = env.mode;
  }

and typedef_constraint env =
  match L.token env.file env.lb with
  | Tword when Lexing.lexeme env.lb = "as" ->
      Some (hint env)
  | _ ->
      L.back env.lb;
      None

and hint_shape_field_list env shape_keyword_pos =
  match L.token env.file env.lb with
  | Tlp -> hint_shape_field_list_remain env
  | _ ->
    L.back env.lb;
    error_at env shape_keyword_pos "\"shape\" is an invalid type; you need to \
    declare and use a specific shape type.";
    []

and hint_shape_field_list_remain env =
  match L.token env.file env.lb with
  | Trp -> []
  | _ ->
      L.back env.lb;
      let error_state = !(env.errors) in
      let fd = hint_shape_field env in
      match L.token env.file env.lb with
      | Trp ->
          [fd]
      | Tcomma ->
          if !(env.errors) != error_state
          then [fd]
          else fd :: hint_shape_field_list_remain env
      | _ ->
          error_expect env ")";
          [fd]

and hint_shape_field env =
  let name = shape_field_name env in
  expect env Tsarrow;
  let ty = hint env in
  name, ty

(*****************************************************************************)
(* Namespaces *)
(*****************************************************************************)

and namespace env =
  (* The safety of the recursive calls here is slightly subtle. Normally, we
   * check for errors when making a recursive call to make sure we don't get
   * stuck in a loop. Here, we actually don't need to do that, since the only
   * time we make a recursive call is when we see (and thus consume) a token
   * that we like. So every time we recurse we'll consume at least one token,
   * so we can't get stuck in an infinite loop. *)
  let tl = match env.mode with
    | FileInfo.Mdecl -> ignore_toplevel ~attr:[]
    | _ -> toplevel in
  (* The name for a namespace is actually optional, so we need to check for
   * the name first. Setting the name to an empty string if there's no
   * identifier following the `namespace` token *)
  let id = match L.token env.file env.lb with
    | Tword -> L.back env.lb; identifier env
    | _ -> L.back env.lb; Pos.make env.file env.lb, "" in
  match L.token env.file env.lb with
  | Tlcb ->
      let body = tl [] env (fun x -> x = Trcb || x = Teof) in
      expect env Trcb;
      id, body
  | Tsc when (snd id) = "" ->
      error_expect env "{";
      id, []
  | Tsc ->
      let terminate = function
        | Tword -> Lexing.lexeme env.lb = "namespace"
        | Teof -> true
        | _ -> false in
      let body = tl [] env terminate in
      id, body
  | _ ->
      error_expect env "{ or ;";
      id, []

and namespace_use_list env acc =
  let p1, s1 = identifier env in
  let id1 = p1, if s1.[0] = '\\' then s1 else "\\" ^ s1 in
  let id2 =
    match L.token env.file env.lb with
    | Tword when Lexing.lexeme env.lb = "as" ->
        identifier env
    | _ ->
        L.back env.lb;
        let str = snd id1 in
        let start = try (String.rindex str '\\') + 1 with Not_found -> 0 in
        let len = (String.length str) - start in
        fst id1, String.sub str start len
  in
  let acc = (id1, id2) :: acc in
  match L.token env.file env.lb with
    | Tsc -> acc
    | Tcomma -> namespace_use_list env acc
    | _ ->
      error_expect env "Namespace use list";
      acc

(*****************************************************************************)
(* Helper *)
(*****************************************************************************)

let from_file file =
  let content =
    try Sys_utils.cat (Relative_path.to_absolute file) with _ -> "" in
  program file content
