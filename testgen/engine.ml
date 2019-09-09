(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Utils = Flowtestgen_utils
module Logging = Flowtestgen_logging
module Config = Flowtestgen_config

(* A virtual class that defines the framework for ocaml-stype rules.

   A rule is an ordinary ocaml function of the type
   env_t -> syntax_t * env_t
   meaning that it will take an environment e, generate some
   syntax and produce a new environment e' by adding more stuffs into
   the old environment. e is used to check the precondition for
   running a rule. If any precondition is not met, the engine will
   raise a Fail exception and it will run other rules to populate the
   environment. For example, if "rule_prop_write" throws a Fail
   exception, we will need to run "rule_empty_object".

   Suppose we have a rule for reading properties to an object:

   has_var(o); obj_type(o); is_number_prop(o, p);
   ------------------------------------
   o.p : number

   We will want to have a rule that first checks
   1. We have a variable o
   2. Var o is of an object type
   3. o has a property p
   4. property p is a number

   After all the preconditions have been satisfied, the engine will
   produce a piece of syntax, "o.p" and also populate the environment
   by adding the expression "o.p" which has type number so that it can
   run other rules later on which use the expression "o.p" as a
   precondition.

   A rule is typically written as a function like this:

   let rule_prop_read env =
     let obj = engine#choose require_var env in
     engine#assert is_obj_type obj;
     let prop = engine#choose require_prop obj in
     engine#assert is_number prop;

     let syntax = mk_syntax in
     let new_env = mk_new_env env in
     syntax, new_env

   The first line contains a call named "require_var" meaning that it
   requires the environment to have a variable. The second line
   asserts that the variable is an object. The engine ensures that the
   rule continues executing iff the assert call passes and in this
   case if the execution passes the second line the "obj" variable
   created in the first line is guaranteed to be an object type. It
   does this by throwing exceptions and reruning the function.

   Another power of the engine comes from the engine#choose method.It
   is very likely that the "obj" variable we get from the environment
   in the first line is not an object and thus the assertion will
   fail.Then a backtracking is exercised to ensure that eventually we
   will get an object from the environment if there's one. The
   engine#choose method does this backtracking behind the
   scene. Whenever an assertion fails, the engine will rerun the rule
   and make sure the results enclosed by engine#choose method will
   return a different result.

   Notice that a rule can be written in many other ways and we could
   totally write the property read rule like this:

   let rule_prop_read env =
     let obj, prop = engine#choose require_obj_with_number_property env in
     let syntax = mk_syntax in
     let new_env = mk_new_env env in
     syntax, new_env

   where the first four lines of the original rule is condensed into a
   single require call. Nothing prevents a user from writing this
   and no backtracking is even required in this case, because a user
   could write "require_obj_with_number_property" in a way that it
   only returns an object variable and its number property. The
   major downside with this style is that it doesn't conform with
   the style of the traditional type rule where preconditions are
   written separately. With the power of backtracking, users could
   write rules with simple require and assert functions and we
   recommend users writing rules this way. *)

(* An exception indicating we want to backtrack *)
exception Backtrack

(* An exception indicating a rule fails *)
exception Fail

(* 'a - type of environment element
   'b - type of the environment
   'c - type of the syntax *)
class virtual ['a, 'b, 'c] engine =
  object (self)
    (* The backtracking is implemented using a hash table
     with a "size" integer which simulates a stack. Whenever
     a user writes a require function or any other functions
     that returns multiple values and backtracking is desired,
     we will put all the candidate values onto the stack.
     Whenever the function at the top of the stack is called,
     we will update the "pointer" so that we will give a different
     candidate value.

     Since a rule can be rerun and thus we could totally run
     a function that is not at the top of the stack. In that case,
     we will simply return its old value, because we need to
     iterate all its children values if we think of the combinatorial
     search in terms of a tree.

     OCaml's stack doesn't provide the functionality of accessing
     the values other than its top. So we are using a hash table
     where the key is an integer serving as an array index if we want
     to access the values other than the top. The "size" value keeps
     track of the size of the stack.
  *)
    val stack = Array.init 100 (fun _ -> [])

    val mutable size = 0

    (* This is used to keep track of the most recent used stack
     level in order to do the correct forward at the right level *)
    val mutable last_stack_lvl = -1

    (* Main methods for getting all the rules for generating programs *)
    method virtual get_all_rules : unit -> ('b -> 'c * 'b) array

    (* The assert function that provides backtracking. A strong
     assertion is guaranteed to be satisfied using backtracking. *)
    method backtrack_on_false (b : bool) : unit = if not b then raise Backtrack

    (* The assert function that will abort a rule *)
    method virtual weak_assert : bool -> unit

    (* Shuffle a list. This is from
     https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml *)
    method shuffle (d : 'a list) : 'a list =
      let nd = Core_list.map ~f:(fun c -> (Random.bits (), c)) d in
      let sond = List.sort compare nd in
      Core_list.map ~f:snd sond

    (* A method for printing the stack *)
    method virtual print_stack : unit -> unit

    method virtual print_env : 'b -> unit

    method virtual print_syntax : 'c -> unit

    method virtual combine_syntax : 'c list -> string

    (* A mehod for getting the name of an engine *)
    method virtual get_name : unit -> string

    (* Choose a element from a list using combinatorial search.

     id : This is index in the stack
     func : This is the function that produces the data

     Whenver this function is called, it will return the "current"
     candidate value. if we encounter a "Backtrack" exception,
     we will call "forward" method to move the "pointer" to the
     next candidate value. If we cannot move the pointer,
     we throws a Fail exception meaning that the rule fails.

     The id param indicates the level in the stack. This is very
     important and necessary, because inside a rule the same
     require functions might be called multiple times. This id
     param ensures that we don't return the same value for
     these identical require functions.
  *)
    method choose (id : int) (func : unit -> 'a list) : 'a =
      (* The depth is larger than the stack right now. We need to
       push the candidate values onto the stack
    *)
      if id >= size then (
        (* push the data onto the stack *)
        stack.(id) <- func ();
        size <- size + 1
      );

      (* Get the current value from the stack *)
      match stack.(id) with
      | [] -> raise Fail
      | hd :: _ ->
        last_stack_lvl <- id;
        hd

    (* We move the pointer forward so that next time "choose" is
     called, we give a different result *)
    method forward () =
      (* The stack is empty. Abort the rule *)
      if size = 0 || last_stack_lvl = -1 then
        raise Fail
      else (
        size <- min (last_stack_lvl + 1) size;

        (* remove the old value *)
        let all_vals = stack.(size - 1) in
        stack.(size - 1) <- List.tl all_vals;

        (* If there's no more new candidate value,
         we pop the function and move the pointer for
         the next level *)
        if stack.(size - 1) = [] then (
          (* pop the empty candidate value list *)
          size <- size - 1;

          (* Move the pointer for the next candidate value list *)
          self#forward ()
        )
      )

    (* Clear the stack *)
    method clear () =
      size <- 0;
      last_stack_lvl <- -1

    (* method for running a single rule *)
    method run (rule : 'b -> 'c * 'b) (env : 'b) : 'c * 'b =
      (* run the rule *)
      try rule env with
      | Backtrack ->
        self#forward ();
        self#run rule env
      | Fail -> raise Fail

    (* Run the rule until we run out of choices *)
    method run_exhaustive (rule : 'b -> 'c * 'b) (env : 'b) : ('c * 'b) list =
      self#clear ();
      let rec helper all_result =
        let r = (try Some (self#run rule env) with Fail -> None) in
        match r with
        | None -> all_result
        | Some r ->
          if size > 0 then
            try
              self#forward ();
              helper (r :: all_result)
            with Fail -> r :: all_result
          else
            r :: all_result
      in
      helper []

    (* Main function for generating programs exhaustively.
     Limit is an integer used to limit the number of programs at the end.
  *)
    method gen_prog (limit : int) : ('c list * 'b) list =
      let rules = self#get_all_rules () in
      (* This is the main queue for storying environments and corresponding syntax *)
      let queue = Queue.create () in
      (* This is used to store temporary results *)
      let tmp_queue = Queue.create () in
      (* We start with empty syntax and empty environment *)
      Queue.push ([], []) queue;

      (* Run a rule through all the results in the queue *)
      let helper (rule : 'b -> 'c * 'b) : unit =
        (*
      Printf.printf "Queue size : %d\n" (Queue.length queue);
      Queue.iter (fun (slist, env) ->
          self#print_env env;
          Printf.printf "Syntax:\n";
          List.iter (fun s -> self#print_syntax s) slist) queue;
       *)
        Queue.iter
          (fun (slist, env) ->
            (* type check the program *)
            let prog = self#combine_syntax slist in
            let type_check_result =
              if Utils.is_typecheck (self#get_name ()) || Config.(config.random) then
                Utils.type_check prog
              else
                None
            in
            match type_check_result with
            | Some msg -> Logging.log_early_type_error prog msg
            | None ->
              let result = self#run_exhaustive rule env in
              if result = [] then
                (* We failed. Put the old syntax and env back into the queue *)
                Queue.push (slist, env) tmp_queue
              else
                List.iter (fun (s, e) -> Queue.push (s :: slist, e) tmp_queue) result)
          queue;

        (* transfer the run results into the queue *)
        Queue.clear queue;
        Queue.transfer tmp_queue queue
      in
      let rec limit_result count acc all_result =
        match all_result with
        | [] -> acc
        | _ when count >= limit -> acc
        | hd :: tl -> limit_result (count + 1) (hd :: acc) tl
      in
      (* Run all the rules *)
      Array.iter (fun rule -> helper rule) rules;

      (* We limit the number of results at the end *)
      Queue.fold (fun acc elt -> elt :: acc) [] queue |> limit_result 0 []
  end
