(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Typing_defs

module SN = Naming_special_names

(*****************************************************************************)
(* Module checking the (co/contra)variance annotations (+/-).
 *
 * The algorithm works in 2 passes:
 *  1) Infer the variance of every type parameter for a given class or typedef
 *  2) Check that the annotation matches the type inferred
 *
 * For every type inferred, we keep a witness (a position in the source),
 * that tells us where the covariance (or contravariance) was deduced.
 * This way, when we find an error, we can point to the place that was
 * problematic (as usual).
 *)
(*****************************************************************************)

(* Type describing the kind of position we are dealing with.
 * Pos.t gives us the position in the source, it doesn't tell us the kind
 * of position we are dealing with. This type keeps track of that.
 *)
type position_descr =
  | Rtypedef
  | Rmember                       (* Instance variable  *)
  | Rtype_parameter               (* The declaration site of a type-parameter
                                   *)
  | Rfun_parameter
  | Rfun_return
  | Rtype_argument of string      (* The argument of a parametric class or
                                   * typedef:
                                   * A<T1, ..>, T1 is (Rtype_argument "A")
                                   *)
  | Rconstraint_as
  | Rconstraint_super

type position_variance =
  | Pcovariant
  | Pcontravariant
  | Pinvariant

type reason = Pos.t * position_descr * position_variance

(* The variance that we have inferred for a given type-parameter. We keep
 * a stack of reasons that made us infer the variance of a position.
 * For example:
 * T appears in foo(...): (function(...): T)
 * T is inferred as covariant + a stack made of two elements:
 * -) The first one points to the position of T
 * -) The second one points to the position of (function(...): T)
 * We can, thanks to this stack, detail why we think something is covariant
 * in the error message.
 *)
type variance =
  (* The type parameter appeared in covariant position. *)
  | Vcovariant     of reason list

  (* The type parameter appeared in contravariant position. *)
  | Vcontravariant of reason list

  (* The type parameter appeared in both covariant and contravariant position.
   * We keep a stack for each side: the left hand side is proof for covariance,
   * while the right hand side is proof for contravariance.
   *)
  | Vinvariant     of reason list * reason list

  (* The type parameter is both covariant and contravariant. This can only
   * happen if the type parameter is not used. Technically, another possible
   * case would be when it is composed with a typedef that is both covariant
   * and contravariant, but since we don't have any syntax to declare such
   * a type, it cannot happen in practice.
   * We don't keep witnesses for such a type, because no error can ever
   * occur.
   *)
  | Vboth

type env = variance SMap.t

(*****************************************************************************)
(* Reason pretty-printing *)
(*****************************************************************************)

let variance_to_string = function
  | Pcovariant     -> "covariant (+)"
  | Pcontravariant -> "contravariant (-)"
  | Pinvariant     -> "invariant"

let variance_to_sign = function
  | Pcovariant     -> "(+)"
  | Pcontravariant -> "(-)"
  | Pinvariant     -> "(I)"

let reason_stack_to_string variance reason_stack =
  Printf.sprintf
    "This position is %s because it is the composition of %s\n\
    The rest of the error messages decomposes the inference of the variance.\n\
    Check out this link if you don't understand what this is about:\n\
    http://en.wikipedia.org/wiki/Covariance_and_contravariance_\
    (computer_science)\
    "
    variance
    begin List.fold_right begin fun (_, _, pvariance) acc ->
      (variance_to_sign pvariance)^acc
    end reason_stack ""
    end

let reason_to_string ~sign (_, descr, variance) =
  (if sign
  then variance_to_sign variance^" "
  else ""
  )^
  match descr with
  | Rtypedef ->
      "Aliased types are covariant"
  | Rmember ->
      "A non private class member is always invariant"
  | Rtype_parameter ->
      "The type parameter was declared as "^variance_to_string variance
  | Rfun_parameter ->
      "Function parameters are contravariant"
  | Rfun_return ->
      "Function return types are covariant"
  | Rtype_argument name ->
      Printf.sprintf
        "This type parameter was declared as %s (cf '%s')"
        (variance_to_string variance)
        name
  | Rconstraint_super ->
      "`super` constraints are covariant"
  | Rconstraint_as ->
      "`as` constraints are contravariant"

let detailed_message variance pos stack =
  match stack with
  | [] -> []
  | [p, _, _ as r] ->
      [p, reason_to_string ~sign:false r]
  | _ ->
      (pos, reason_stack_to_string variance stack) ::
      List.map (fun (p, _, _ as r) -> p, reason_to_string ~sign:true r) stack

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)

let to_string = function
  | Vcovariant _     -> "Vcovariant"
  | Vcontravariant _ -> "Vcontravariant"
  | Vinvariant _     -> "Vinvariant"
  | Vboth            -> "Vboth"

(*****************************************************************************)
(* Converts an annotation (+/-) to a type. *)
(*****************************************************************************)

let make_variance reason pos = function
  | Ast.Covariant ->
      Vcovariant [pos, reason, Pcovariant]
  | Ast.Contravariant ->
      Vcontravariant [pos, reason, Pcontravariant]
  | Ast.Invariant ->
      Vinvariant ([pos, reason, Pinvariant], [pos, reason, Pinvariant])

(*****************************************************************************)
(* Adds a new usage of a type to the environment.
 * This is not as simple as SMap.add, because, depending on what was already
 * there, a type can become invariant.
 * Let's say we start with T used in a covariant position: the type should
 * be Vcovariant.
 * Now if we find a place where T is used in contravariant position, it
 * must become invariant.
 *)
(*****************************************************************************)

let add_variance env name variance =
  match SMap.get name env with
  | None -> SMap.add name variance env
  | Some old_variance ->
      match old_variance, variance with
      (* Whatever the type is, if we add a usage that is both (covariant and
       * contravariant) then the variance is unchanged.
       *)
      | _, Vboth
      (* The usage is already invariant (the most restrictive type), it
       * cannot change past this point.
       *)
      | Vinvariant _, _ -> env
      (* The type is both (covariant and contravariant), Whatever the new
       * usage is, it's going to be more restrictive than what we already
       * have.
       *)
      | Vboth, _
      (* We are adding an invariant case (the most restrictive variance),
       * whatever the previous cases were, invariant is the new type
       *)
      | _, Vinvariant _ -> SMap.add name variance env
      | Vcovariant _, Vcovariant _
      | Vcontravariant _, Vcontravariant _ -> env
      | Vcovariant p1, Vcontravariant p2 ->
          SMap.add name (Vinvariant (p1, p2)) env
      | Vcontravariant p2, Vcovariant p1 ->
          SMap.add name (Vinvariant (p1, p2)) env

(*****************************************************************************)
(* Used when we want to compose with the variance coming from another class.
 * Let's assume: A<-T>
 * public function foo(A<T> $x): void;
 * A<T> is in contravariant position so we process -A<T>.
 * because A is annotated with a minus: we deduce --T (which becomes T).
 *)
(*****************************************************************************)

let compose (pos, param_descr) from to_ =
  (* We don't really care how we deduced the variance that we are composing
   * with (_stack_to). That's because the decomposition could be in a different
   * file and would be too hard to follow anyway.
   * Let's consider the following return type: A<T>.
   * Turns out A is declared as A<-T>.
   * It's not a good idea for us to point to what made us deduce that the
   * position was contravariant (the declaration side). Because the user will
   * wonder where it comes from.
   * It's better to point to the return type (more precisely, point to the T
   * in the return type) and explain that this position is contravariant.
   * Later on, the user can go and check the definition of A for herself.
   *)
  match from, to_ with
  | Vcovariant stack_from, Vcovariant _stack_to ->
      let reason = pos, param_descr, Pcovariant in
      Vcovariant (reason :: stack_from)
  | Vcontravariant stack_from, Vcontravariant _stack_to ->
      let reason = pos, param_descr, Pcontravariant in
      Vcovariant (reason :: stack_from)
  | Vcovariant stack_from, Vcontravariant _stack_to ->
      let reason = pos, param_descr, Pcontravariant in
      Vcontravariant (reason :: stack_from)
  | Vcontravariant stack_from, Vcovariant _stack_to ->
      let reason = pos, param_descr, Pcovariant in
      Vcontravariant (reason :: stack_from)
  | (Vinvariant _ as x), _ -> x
  | _, Vinvariant (_co, _contra) ->
      let reason = pos, param_descr, Pinvariant in
      Vinvariant ([reason], [reason])
  | Vboth, x | x, Vboth -> x

(*****************************************************************************)
(* Used for the arguments of function. *)
(*****************************************************************************)

let flip reason = function
  | Vcovariant stack -> Vcontravariant (reason :: stack)
  | Vcontravariant stack -> Vcovariant (reason :: stack)
  | Vinvariant _ as x -> x
  | Vboth -> Vboth

(*****************************************************************************)
(* Given a type parameter, returns the variance inferred. *)
(*****************************************************************************)

let get_tparam_variance env (_, (_, tparam_name), _) =
  match SMap.get tparam_name env with
  | None -> Vboth
  | Some x -> x

(*****************************************************************************)
(* Given a type parameter, returns the variance declared. *)
(*****************************************************************************)

let make_tparam_variance (variance, (pos, _), _) =
  make_variance Rtype_parameter pos variance

(*****************************************************************************)
(* Checks that the annotation matches the inferred type. *)
(*****************************************************************************)

let check_variance env tparam =
  let declared_variance = make_tparam_variance tparam in
  let inferred_variance = get_tparam_variance env tparam in
  match declared_variance, inferred_variance with
  | Vboth, _ -> assert false (* There is no syntax for that *)
  | _, Vboth -> ()
  | Vinvariant _, _ -> ()
  | Vcovariant _, Vcovariant _
  | Vcontravariant _, Vcontravariant _ -> ()
  | Vcovariant stack1, (Vcontravariant stack2 | Vinvariant (_, stack2)) ->
      let (pos1, _, _) = List.hd stack1 in
      let (pos2, _, _) = List.hd stack2 in
      let emsg = detailed_message "contravariant (-)" pos2 stack2 in
      Errors.declared_covariant pos1 pos2 emsg
  | Vcontravariant stack1, (Vcovariant stack2 | Vinvariant (stack2, _)) ->
      let (pos1, _, _) = List.hd stack1 in
      let (pos2, _, _) = List.hd stack2 in
      let emsg = detailed_message "covariant (+)" pos2 stack2 in
      Errors.declared_contravariant pos1 pos2 emsg

(*****************************************************************************)
(* Returns the list of type parameter variance for a given class.
 * Performing that operation adds a dependency on the class, because if
 * the class changes (especially the variance), we must re-check the class.
 *
 * N.B.: this function works both with classes and typedefs.
 *)
(*****************************************************************************)

let get_class_variance root (pos, class_name) =
  match class_name with
  | name when (name = SN.Classes.cAwaitable) ->
      [Vcovariant [pos, Rtype_argument (Utils.strip_ns name), Pcovariant]]
  | _ ->
      let dep = Typing_deps.Dep.Class class_name in
      Typing_deps.add_idep (Some root) dep;
      let tparams =
        if Typing_heap.Typedefs.mem class_name
        then
          match Typing_heap.Typedefs.get class_name with
          | Some (Typing_heap.Typedef.Ok (_, tparams, _, _, _)) -> tparams
          | _ -> []
        else
          match Typing_heap.Classes.get class_name with
          | None -> []
          | Some { tc_tparams; _ } -> tc_tparams
      in
      List.map make_tparam_variance tparams

(*****************************************************************************)
(* The entry point (for classes). *)
(*****************************************************************************)

let rec class_ class_name class_type impl =
  let root = Typing_deps.Dep.Class class_name in
  let tparams = class_type.tc_tparams in
  let env = SMap.empty in
  let env = List.fold_left (type_ root Vboth) env impl in
  let env = SMap.fold (class_member root) class_type.tc_props env in
  let env = SMap.fold (class_method root) class_type.tc_methods env in
  List.iter (check_variance env) tparams

(*****************************************************************************)
(* The entry point (for typedefs). *)
(*****************************************************************************)

and typedef type_name =
  match Typing_heap.Typedefs.get type_name with
  | Some (Typing_heap.Typedef.Ok (_, tparams, _cstr, ty, _)) ->
      let root = Typing_deps.Dep.Class type_name in
      let env = SMap.empty in
      let pos = Reason.to_pos (fst ty) in
      let reason_covariant = [pos, Rtypedef, Pcovariant] in
      let env = type_ root (Vcovariant reason_covariant) env ty in
      List.iter (check_variance env) tparams
  | _ -> ()

and class_member root _member_name member env =
  match member.ce_visibility with
  | Vprivate _ -> env
  | _ ->
      let reason, _ as ty = member.ce_type in
      let pos = Reason.to_pos reason in
      let variance = make_variance Rmember pos Ast.Invariant in
      type_ root variance env ty

and class_method root _method_name method_ env =
  match method_.ce_visibility with
  | Vprivate _ -> env
  | _ ->
      match method_.ce_type with
      | _, Tfun { ft_tparams; ft_params; ft_ret; _ } ->
          let env = List.fold_left begin fun env (_, (_, tparam_name), _) ->
            SMap.remove tparam_name env
          end env ft_tparams in
          let env = List.fold_left (fun_param root) env ft_params in
          let env = fun_ret root env ft_ret in
          env
      | _ -> assert false

and fun_param root env (_, (reason, _ as ty)) =
  let pos = Reason.to_pos reason in
  let reason_contravariant = pos, Rfun_parameter, Pcontravariant in
  let variance = Vcontravariant [reason_contravariant] in
  type_ root variance env ty

and fun_ret root env (reason, _ as ty) =
  let pos = Reason.to_pos reason in
  let reason_covariant = pos, Rfun_return, Pcovariant in
  let variance = Vcovariant [reason_covariant] in
  type_ root variance env ty

and type_option root variance env = function
  | None -> env
  | Some ty -> type_ root variance env ty

and type_list root variance env tyl =
  List.fold_left (type_ root variance) env tyl

and type_ root variance env (reason, ty) =
  match ty with
  | Tany -> env
  | Tmixed -> env
  | Tarray (ty1, ty2) ->
    let env = type_option root variance env ty1 in
    let env = type_option root variance env ty2 in
    env
  | Tthis ->
      (* `this` constraints are bivariant (otherwise any class that used the
       * `this` type would not be able to use covariant type params) *)
      env
  | Tgeneric (_, Some cstr) -> constraint_ root env cstr
  | Tgeneric (name, None) ->
      let pos = Reason.to_pos reason in
      (* This section makes the position more precise.
       * Say we find a return type that is a tuple (int, int, T).
       * The whole tuple is in covariant position, and so the position
       * is going to include the entire tuple.
       * That can make things pretty unreadable when the type is long.
       * Here we replace the position with the exact position of the generic
       * that was problematic.
       *)
      let variance =
        match variance with
        | Vcovariant ((pos', x, y) :: rest) when pos <> pos' ->
            Vcovariant ((pos, x, y) :: rest)
        | Vcontravariant ((pos', x, y) :: rest) when pos <> pos' ->
            Vcontravariant ((pos, x, y) :: rest)
        | x -> x
      in
      let env = add_variance env name variance in
      env
  | Toption ty ->
      type_ root variance env ty
  | Tprim _ -> env
  | Tfun ft ->
      let env = List.fold_left begin fun env (_, (r, _ as ty)) ->
        let pos = Reason.to_pos r in
        let reason = pos, Rfun_parameter, Pcontravariant in
        let variance = flip reason variance in
        type_ root variance env ty
      end env ft.ft_params
      in
      let ret_pos = Reason.to_pos (fst ft.ft_ret) in
      let ret_variance = ret_pos, Rfun_return, Pcovariant in
      let variance =
        match variance with
        | Vcovariant stack -> Vcovariant (ret_variance :: stack)
        | Vcontravariant stack -> Vcontravariant (ret_variance :: stack)
        | variance -> variance
      in
      let env = type_ root variance env ft.ft_ret in
      env
  | Tapply (_, []) -> env
  | Tapply ((_, name as pos_name), tyl) ->
      let variancel = get_class_variance root pos_name in
      wfold_left2 begin fun env tparam_variance (r, _ as ty) ->
        let pos = Reason.to_pos r in
        let reason = Rtype_argument (Utils.strip_ns name) in
        let variance = compose (pos, reason) variance tparam_variance in
        type_ root variance env ty
      end env variancel tyl
  | Ttuple tyl ->
      type_list root variance env tyl
  (* when we add type params to type consts might need to change *)
  | Taccess _ -> env
  | Tshape (_, ty_map) ->
      Nast.ShapeMap.fold begin fun _field_name ty env ->
        type_ root variance env ty
      end ty_map env

(* `as` constraints must be contravariant and `super` constraints covariant. To
 * see why, suppose that we allow the wrong variance:
 *
 *   class Foo<+T> {
 *     public function bar<Tu as T>(Tu $x) {}
 *   }
 *
 * Let A and B be classes, with B a subtype of A. Then
 *
 *   function f(Foo<A> $x) {
 *     $x->(new A());
 *   }
 *
 * typechecks. However, covariance means that we could call `f()` with an
 * instance of B. However, B::bar would expect its argument $x to be a subtype
 * of B, but we would be passing an instance of A to it, which should be a type
 * error. In other words, `as` constraints are upper type bounds, and since
 * subtypes must have more relaxed constraints than their supertypes, `as`
 * constraints must be contravariant. (Reversing this argument shows that
 * `super` constraints must be covariant.)
 *
 * The preceding discussion might lead one to think that the constraints should
 * have the same variance as the class parameter types, i.e. that `as`
 * constraints used in the return type should be covariant. In particular,
 * suppose
 *
 *   class Foo<-T> {
 *     public function bar<Tu as T>(Tu $x): Tu { ... }
 *     public function baz<Tu as T>(): Tu { ... }
 *   }
 *
 *   class A {}
 *   class B extends A {
 *     public function qux() {}
 *   }
 *
 *   function f(Foo<B> $x) {
 *     $x->baz()->qux();
 *   }
 *
 * Now `f($x)` could be a runtime error if `$x` was an instance of Foo<A>, and
 * it seems like we should enforce covariance on Tu. However, the real problem
 * is that constraints apply to _instantiation_, not usage. As far as Hack
 * knows, $x->bar() could be of any type, since we have not provided any clues
 * about how `Tu` should be instantiated. In fact `$x->bar()->whatever()`
 * succeeds as well, because `Tu` is of type Tany -- though in an ideal world
 * we would make it Tmixed in order to ensure soundness. Also, the signature
 * of `baz()` doesn't entirely make sense -- why constrain Tu if it it's only
 * getting used in one place?
 *
 * Thus, if one wants type safety, how `$x` _should_ be used is
 *
 *   function f(Foo<B> $x) {
 *     $x->bar(new B()))->qux();
 *   }
 *
 * Thus we can see that, if `$x` is used correctly (or if we enforced correct
 * use by returning Tmixed for uninstantiated type variables), we would always
 * know the exact type of Tu, and Tu can be validly used in both co- and
 * contravariant positions.
 *
 * Remark: This is far more intuitive if you think about how Vector::concat is
 * typed with its `Tu super T` type in both the parameter and return type
 * positions. Uninstantiated `super` types are less likely to cause confusion,
 * however -- you can't imagine doing very much with a returned value that is
 * some (unspecified) supertype of a class.
 *)
and constraint_ root env cstr =
  match cstr with
  | Ast.Constraint_as, (r, _ as ty) ->
      let pos = Reason.to_pos r in
      let reason = pos, Rconstraint_as, Pcontravariant in
      type_ root (Vcontravariant [reason]) env ty
  | Ast.Constraint_super, (r, _ as ty) ->
      let pos = Reason.to_pos r in
      let reason = pos, Rconstraint_super, Pcovariant in
      type_ root (Vcovariant [reason]) env ty
