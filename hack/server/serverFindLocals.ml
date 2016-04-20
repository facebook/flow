(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast
open Core

type result = Pos.absolute list

module PosSet = Set.Make(Pos)

module LocalPositions = struct
  (**
   * Local positions is a map from an identifier -- a unique integer that
   * identifies a local "symbol" -- to a set of all known positions where
   * that local is used.
   *
   * Identifiers that identify no known local produce an empty set of
   * positions.
   *)
  type t = PosSet.t IMap.t

  let empty = IMap.empty

  let get ident locals =
    let current = IMap.get ident locals in
    Option.value ~default:PosSet.empty current

  let add ident pos locals =
    let positions = get ident locals in
    IMap.add ident (PosSet.add pos positions) locals
end (* End of module LocalPositions *)

module ScopeChain = struct
  (**
   * A scope maps from a string (the text of the use of a local) to an
   * ident (a unique integer associated with this local symbol).
   *
   * A scope chain is a stack of scopes. If there is a match at the head
   * of the stack, that match shadows any matches in the tail. Otherwise,
   * the tail is checked.
   *)
  type scope = Ident.t SMap.t
  type t = scope list

  let empty = []

  let push scopechain =
    SMap.empty :: scopechain

  let pop scopechain =
    match scopechain with
    | _ :: t -> t
    | _ -> failwith "popping empty scope chain"

  let add name ident scopechain =
    match scopechain with
    | h :: t -> (SMap.add name ident h) :: t
    | _ -> failwith "adding name to empty scope chain"

  let rec get name scopechain =
    match scopechain with
    | [] -> None
    | h :: t ->
      let result = SMap.get name h in
      if result = None then get name t
      else result
end (* End of module ScopeChain *)

module ScopeChains = struct
(**
  * A "scope chain" is a stack of maps from names to idents. You might
  * think that would be sufficient, but unfortunately it is not.
  *
  * When we have a nested function (not a lambda), then the scope chain
  * from outside the nested function is not accessible inside the nested
  * function. Rather, a new scope chain is created which has on it the
  * formal parameters and the variables inherited from the outer scope chain
  * by the "use" clause. We therefore have a stack of scope chains.
  *)

  type t = ScopeChain.t list

  let head scopechains =
    match scopechains with
    | h :: _ -> h
    | _ -> failwith "Scope chain stack is empty"

  let previous scopechains =
    match scopechains with
    | _ :: p :: _ -> p
    | _ -> failwith "Scope chain stack is too small"

  let pop scopechains =
    match scopechains with
    | _ :: t -> t
    | _ -> failwith "Scope chain stack is empty"

  let replace_head scopechain scopechains  =
    scopechain :: (pop scopechains)

  let push scopechain scopechains =
    scopechain :: scopechains
end (* End of module ScopeChains *)

module LocalMap = struct
  (**
   * A "local map" is a scope chain stack and a local positions map.
   * When a usage of a local is encountered, there are several possibilities
   * for what has to happen next.
   *
   * (1) The most common scenario is: we've seen this local before. We look
   * it up in the current scope chain, get a hit, and move on.
   *
   * (2) If this is the first time we've seen this perfectly ordinary local
   * then we make a new identifer and add it to the scope chain.
   *
   * (3) There are some situations where we know we must introduce a new ident
   * even if the name is in scope. Those situations include: a local is
   * introduced by a formal parameter, a local is introduced by a catch
   * clause, a function static shadows an existing local.
   *
   * (4) Finally, we need to do a lot of work to correctly handle a nested
   * anonymous function with a "use" clause. See below for the details.
   *
   * Regardless, we add the position to the local positions map.
   *
   * We have a particular target local in mind but we only know its position.
   * When we encounter a local at that position, we'll make a note of the
   * ident so that we can look up the associated positions later.
   *)
  type t = {
    scopechains : ScopeChains.t;
    locals : LocalPositions.t;
    target_line : int;
    target_char : int;
    target_ident : Ident.t option
  }

  let make target_line target_char = {
    scopechains = ScopeChain.empty :: [];
    locals = LocalPositions.empty;
    target_line;
    target_char;
    target_ident = None
  }

  let current_scopechain localmap =
    ScopeChains.head localmap.scopechains

  let previous_scopechain localmap =
    ScopeChains.previous localmap.scopechains

  let previous_scopechains localmap =
    ScopeChains.pop localmap.scopechains

  let replace_head scopechain localmap =
    let scopechains =
      ScopeChains.replace_head scopechain localmap.scopechains in
    { localmap with scopechains }

  let push localmap =
    let scopechain = ScopeChain.push (current_scopechain localmap) in
    replace_head scopechain localmap

  let pop localmap =
    let scopechain = ScopeChain.pop (current_scopechain localmap) in
    replace_head scopechain localmap

  let push_scopechain localmap =
    let scopechains = ScopeChains.push ScopeChain.empty localmap.scopechains in
    { localmap with scopechains }

  let pop_scopechain localmap =
    let scopechains = ScopeChains.pop localmap.scopechains in
    { localmap with scopechains }

  let overlaps pos localmap =
    let pos_line, pos_start, pos_end = Pos.info_pos pos in
    pos_line = localmap.target_line &&
    pos_start <= localmap.target_char &&
    localmap.target_char <= pos_end

  let get_target_ident ident pos localmap =
    if localmap.target_ident = None && overlaps pos localmap then
      Some ident
    else
      localmap.target_ident

  (**
    * This version of add always overwrites the existing scope entry, even
    * if there's a local of this name already in scope. We need this for:
    *
    * function ($x) {
    *   $y = $x;
    *   static $x = 123;
    *   $z = $x;
    * }
    *
    * We have two variables both named $x, one formal and one static. The
    * later comes into scope at the point of its declaration, and is in
    * scope for the rest of the body. The easiest way to model this is to
    * simply replace the existing scope entry, if there is one.
    *)
  let force_add name pos localmap =
    let ident = Ident.make name in
    let current = current_scopechain localmap in
    let scopechain = ScopeChain.add name ident current in
    let scopechains =
      ScopeChains.replace_head scopechain localmap.scopechains in
    let locals = LocalPositions.add ident pos localmap.locals in
    let target_ident = get_target_ident ident pos localmap in
    { localmap with scopechains; locals; target_ident }

  (**
    * When we encounter a |> operator, we need to push a scope on the right
    * side and add a brand-new $$ to that scope, but we do not yet have a
    * position for it!
    *)
  let add_dollardollar localmap =
    let name = "$$" in
    let ident = Ident.make name in
    let current = current_scopechain localmap in
    let scopechain = ScopeChain.add name ident current in
    let scopechains =
      ScopeChains.replace_head scopechain localmap.scopechains in
    { localmap with scopechains }

  (* Returns an (ident, bool) where the bool is true if the ident was found. *)
  let find_or_create_ident name localmap =
    let current = current_scopechain localmap in
    let result = ScopeChain.get name current in
    match result with
    | None -> (Ident.make name, false)
    | Some ident -> (ident, true)

  (**
   * This version of add only adds an entry to the current scope if we haven't
   * got a binding for this name.
   *)
  let add name pos localmap =
    let ident, found = find_or_create_ident name localmap in
    let scopechains =
      if found then
        localmap.scopechains
      else begin
        let current = current_scopechain localmap in
        let scopechain = ScopeChain.add name ident current in
        ScopeChains.replace_head scopechain localmap.scopechains end in
    let locals = LocalPositions.add ident pos localmap.locals in
    let target_ident = get_target_ident ident pos localmap in
    { localmap with scopechains; locals; target_ident }

  let add_from_use name pos localmap =
    (**
     * When adding an item from a use clause we've got to do a couple
     * things. First, the variables mentioned in the use clause are
     * mentions of those variables, so they need to go in the list of
     * positions. Second, the binding from the name to its id from the
     * _previous_ scopechain needs to be copied into the _current_
     * scopechain. *)
    let current = current_scopechain localmap in
    let previous = previous_scopechain localmap in
    let canonical = ScopeChain.get name previous in
    (* The user may have given a variable in the use clause which does not
       exist. In that case, just make up a new ident. *)
    let ident = match canonical with
    | None -> Ident.make name
    | Some ident -> ident in
    (* Regardless, this is a mention of that local. *)
    let locals = LocalPositions.add ident pos localmap.locals in
    let scopechain = ScopeChain.add name ident current in
    let scopechains =
      ScopeChains.replace_head scopechain localmap.scopechains in
    let target_ident = get_target_ident ident pos localmap in
    { localmap with scopechains; locals; target_ident }

  let results localmap =
    let results_set = match localmap.target_ident with
    | Some ident -> LocalPositions.get ident localmap.locals
    | _ -> PosSet.empty in
    let results_list = PosSet.elements results_set in
    List.map results_list Pos.to_absolute
end (* End of module LocalMap *)

(**
  * Here begins the code which walks the AST and accumulates the positions
  * of every local in this file.
  *)

class local_finding_visitor = object(this)
  inherit [LocalMap.t] Ast_visitor.ast_visitor as super

  method! on_lvar localmap (pos, name) =
    LocalMap.add name pos localmap

  method! on_expr localmap (pos, e) =
    match e with
    | Dollardollar -> LocalMap.add "$$" pos localmap
    | _ -> super#on_expr localmap (pos, e)

  method! on_pipe localmap left right =
    (**
      A pipe expression has a left side and a right side. It introduces a
      new scope on the right side only which defines a new magic local
      called $$, which is equal to the value of the left side.

      Consider for example:

      ( a |> b($$) ) |> ( c((d($$) |> e($$)), $$) )
          1    1     2         2   3    3     2

      Here I have numbered all the pipe operators and $$s, indicating which
      $$ is in scope at each usage.

      TODO: ericlippert
      TODO: What is the interaction between the pipe operator and nested
      TODO: anonymous functions? Does the $$ automatically end up in scope
      TODO: inside the body, or does it have to go in a use($$) clause?
      TODO: If the former, then we'll need to explicitly copy the active $$
      TODO: binding into the scope chain introduced; if the latter then
      TODO: the existing mechanisms should be sufficient.  Either way,
      TODO: this should be tested.

      *)
    let localmap = this#on_expr localmap left in
    let localmap = LocalMap.push localmap in
    let localmap = LocalMap.add_dollardollar localmap in
    let localmap = this#on_expr localmap right in
    LocalMap.pop localmap

  method! on_method_ localmap m =
    let localmap = LocalMap.push localmap in
    let localmap =
      List.fold_left m.m_params ~init:localmap ~f:this#on_fun_param in
    let localmap = this#on_block localmap m.m_body in
    LocalMap.pop localmap

  (**
    * This is called for both top-level functions and lambdas, but not
    * anonymous functions.
    *)
  method! on_fun_ localmap f =
    let localmap = LocalMap.push localmap in
    let localmap =
      List.fold_left f.f_params ~init:localmap ~f:this#on_fun_param in
    let localmap = this#on_block localmap f.f_body in
    LocalMap.pop localmap

  method! on_fun_param localmap p =
      (**
        * A formal parameter always introduces a new local into the current
        * scope; we never want to unify a formal with a local seen previously.
        *)
      let pos, name = p.param_id in
      LocalMap.force_add name pos localmap

  method! on_efun localmap fn use_list =
   (**
     * This is a traditional PHP nested function, and this is a bit tricky.
     * Consider first a normal case:

     function test1($x, $y) {
       $f = function ($z) use($x) {
         $y = 100;
         return $x + $y + $z;
       };
       return $f(10);
     }

     * We must detect every use of $x as a use of the formal parameter,
     * including the usage inside the "use". We must *not* detect use of
     * $y as a use of the formal parameter $y, since it was not mentioned
     * in the use clause.
     *
     * Now consider a corner case:

     function test2() {
       $x = 20;
       $f = function ($x) use ($x) {
         return $x;
       };
       return $f(10);
     }

     * Which $x is returned by the nested function? We have both a formal
     * and a closed-over local to choose from. In this case the
     * formal wins.
     *
     * So, what's the deal here? How do we want to structure this?
     *
     * Consider test1.  The local map at the point where we are
     * about to evaluate the nested function is:
     *
     * scopechains:
     [
       [
         [ $x -> 1; $y -> $2; $f -> 3 ]
       ]
     ]
     *
     * We will push a new scopechain so that $y comes out of scope, then
     * push *two* scopes, one for the using, and one for the formals:
     *
     * scopechains:
     [
       [
         [ $z -> 4 ]; [ $x -> 1 ]
       ];
       [
         [ $x -> 1; $y -> $2; $f -> 3 ]
       ]
     ]
     *
     * Now a lookup of $z will go to the head of the current scope chain
     * stack. A lookup of $x will go the tail. And a lookup of $y will
     * fail, because the tail of the scope chain stack is not searched.
     * When $y is encountered it will produce:
     * scopechains:
     [
       [
         [ $y -> 5; $z -> 4 ]; [ $x -> 1 ]
       ];
       [
         [ $x -> 1; $y -> $2; $f -> 3 ]
       ]
     ]
     *
     * What about test2? We will have
     * scopechains:
     [
       [
         [ $x -> 2 ]; [ $x -> 1 ]
       ];
       [
         [ $x -> 1 ]
       ]
     ]
     *
     * So a lookup of $x in the body will produce the formal.
     *
     * All right, let's proceed.

     *)

     let localmap = LocalMap.push_scopechain localmap in
     let localmap = LocalMap.push localmap in
     (* No need to pop; we're going to pop the whole scopechain. *)
     let localmap = List.fold_left use_list ~init:localmap
       ~f:begin fun l ((p, n), _) -> LocalMap.add_from_use n p l end in
     let localmap = this#on_fun_ localmap fn in
     LocalMap.pop_scopechain localmap

   method on_static_variable localmap static =
     (**
       * Introducing a static variable hides any existing variable in this
       * scope of the same name. We model this by simply replacing the existing
       * scope entry with the new one. See commentary to force_add, above.
       *
       * There are two forms we are concerned with:
       *
       * static $x;
       * static $x = 123;
       *
       * If we have neither expected form then we do not attempt to add a
       * local to the map.
       *
       * Note that in the second form we do scan the initializer looking for
       * locals; if the program has one then it is an error because the
       * initializer must be a constant. But we should look for the use of a
       * local because that is still a usage of the local! One imagines that
       * the IDE could for instance detect something like
       *
       * static $a = 123;
       * static $b = $a;
       *
       * and fix up the error appropriately. We cannot do that if we cannot
       * find the local inside the initializer.
       *)
     match static with
     | _, Lvar (pos, name) -> LocalMap.force_add name pos localmap
     | _, Binop(Eq None, (_, Lvar (pos, name)), init) ->
       let localmap = LocalMap.force_add name pos localmap in
       this#on_expr localmap init
     | _ -> localmap

  method! on_static_var localmap statics =
    List.fold_left statics ~init:localmap
      ~f:begin fun l s -> this#on_static_variable l s end

  method! on_catch localmap (_, (pos, name), body) =
    (**
      * A catch block creates a local which is only in scope for the
      * body of the catch.
      *
      * The type name should never reasonably contain a local; we'll ignore it.
      *)
    let localmap = LocalMap.push localmap in
    let localmap = LocalMap.force_add name pos localmap in
    let localmap = this#on_block localmap body in
    LocalMap.pop localmap
end

let parse content =
  Errors.ignore_ begin fun () ->
    let {Parser_hack.file_mode; comments; ast} =
      Parser_hack.program Relative_path.default content
    in ast
  end

(**
  * This is the entrypoint to this module. The contents of a file, and a
  * position within it, identifying a local, are given. The result is a
  * list of the positions of other uses of that local in the file.
  *)
let go content line char =
  let ast = parse content in
  let empty = LocalMap.make line char in
  let visitor = new local_finding_visitor in
  let localmap = visitor#on_program empty ast in
  LocalMap.results localmap
