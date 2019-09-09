(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty_symbol

class ['self] iter_ty_base =
  object (_ : 'self)
    method private on_string : 'env. 'env -> string -> unit = (fun _env _x -> ())

    method private on_bool : 'env. 'env -> bool -> unit = (fun _env _x -> ())

    method private on_int : 'env. 'env -> int -> unit = (fun _env _x -> ())

    method private on_symbol : 'env. 'env -> symbol -> unit = (fun _env _x -> ())

    method private on_aloc : 'env. 'env -> ALoc.t -> unit = (fun _env _x -> ())

    method private on_option : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option =
      (fun f env -> Option.map ~f:(f env))

    method private on_list : 'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit =
      (fun f env -> List.iter (f env))
  end

class ['self] iter2_ty_base =
  object (self : 'self)
    method private on_string : 'env. 'env -> string -> string -> unit = (fun _env _x _y -> ())

    method private on_bool : 'env. 'env -> bool -> bool -> unit = (fun _env _x _y -> ())

    method private on_int : 'env. 'env -> int -> int -> unit = (fun _env _x _y -> ())

    method private on_symbol : 'env. 'env -> symbol -> symbol -> unit = (fun _env _x _y -> ())

    method private on_aloc : 'env. 'env -> ALoc.t -> ALoc.t -> unit = (fun _env _x _y -> ())

    method private fail_option : 'env 'a. 'env -> 'a option -> 'a option -> unit =
      (fun _ _ _ -> raise VisitorsRuntime.StructuralMismatch)

    method private on_option
        : 'env 'a. ('env -> 'a -> 'a -> unit) -> 'env -> 'a option -> 'a option -> unit =
      fun f env x y ->
        match (x, y) with
        | (Some x, Some y) -> f env x y
        | (Some _, None)
        | (None, Some _) ->
          self#fail_option env x y
        | (None, None) -> ()

    method private fail_list : 'env 'a. 'env -> 'a list -> 'a list -> unit =
      (fun _ _ _ -> raise VisitorsRuntime.StructuralMismatch)

    method private on_list
        : 'env 'a. ('env -> 'a -> 'a -> unit) -> 'env -> 'a list -> 'a list -> unit =
      fun f env l1 l2 ->
        match (l1, l2) with
        | ([], []) -> ()
        | (a1 :: l1, a2 :: l2) ->
          f env a1 a2;
          self#on_list f env l1 l2
        | (l1, l2) -> self#fail_list env l1 l2
  end

class ['self] map_ty_base =
  object (_ : 'self)
    method private on_string : 'env -> string -> string = (fun _ x -> x)

    method private on_bool : 'env -> bool -> bool = (fun _ x -> x)

    method private on_int : 'env -> int -> int = (fun _ x -> x)

    method private on_symbol : 'env -> symbol -> symbol = (fun _ x -> x)

    method private on_aloc : 'env -> ALoc.t -> ALoc.t = (fun _ x -> x)

    method private on_list : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
      (fun f env -> Core_list.map ~f:(f env))

    method private on_option : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option =
      (fun f env -> Option.map ~f:(f env))
  end

class ['self] endo_ty_base =
  object (_self : 'self)
    method private on_string : 'env -> string -> string = (fun _ x -> x)

    method private on_bool : 'env -> bool -> bool = (fun _ x -> x)

    method private on_int : 'env -> int -> int = (fun _ x -> x)

    method private on_symbol : 'env -> symbol -> symbol = (fun _ x -> x)

    method private on_aloc : 'env -> ALoc.t -> ALoc.t = (fun _ x -> x)

    (* Copied from
     * https://github.com/facebook/hhvm/blob/master/hphp/hack/src/ast/ast_defs_visitors_ancestors.ml
     *)
    method private on_list : 'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a list -> 'a list =
      fun f env xs ->
        let rec aux env xs counter =
          match xs with
          | [] -> xs
          | [y1] ->
            let z1 = f env y1 in
            if y1 == z1 then
              xs
            else
              [z1]
          | [y1; y2] ->
            let z1 = f env y1 in
            let z2 = f env y2 in
            if y1 == z1 && y2 == z2 then
              xs
            else
              [z1; z2]
          | [y1; y2; y3] ->
            let z1 = f env y1 in
            let z2 = f env y2 in
            let z3 = f env y3 in
            if y1 == z1 && y2 == z2 && y3 == z3 then
              xs
            else
              [z1; z2; z3]
          | [y1; y2; y3; y4] ->
            let z1 = f env y1 in
            let z2 = f env y2 in
            let z3 = f env y3 in
            let z4 = f env y4 in
            if y1 == z1 && y2 == z2 && y3 == z3 && y4 == z4 then
              xs
            else
              [z1; z2; z3; z4]
          | [y1; y2; y3; y4; y5] ->
            let z1 = f env y1 in
            let z2 = f env y2 in
            let z3 = f env y3 in
            let z4 = f env y4 in
            let z5 = f env y5 in
            if y1 == z1 && y2 == z2 && y3 == z3 && y4 == z4 && y5 == z5 then
              xs
            else
              [z1; z2; z3; z4; z5]
          | y1 :: y2 :: y3 :: y4 :: y5 :: ys ->
            let z1 = f env y1 in
            let z2 = f env y2 in
            let z3 = f env y3 in
            let z4 = f env y4 in
            let z5 = f env y5 in
            let zs =
              if counter > 1000 then
                aux_slow env ys [] ys false
              else
                aux env ys (counter + 1)
            in
            if y1 == z1 && y2 == z2 && y3 == z3 && y4 == z4 && y5 == z5 && ys == zs then
              xs
            else
              z1 :: z2 :: z3 :: z4 :: z5 :: zs
        and aux_slow env xs acc original_list has_new_elements =
          match xs with
          | [] ->
            if has_new_elements then
              List.rev acc
            else
              original_list
          | y1 :: ys ->
            let z1 = f env y1 in
            aux_slow env ys (z1 :: acc) original_list (has_new_elements || y1 != z1)
        in
        aux env xs 0

    method private on_option : 'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a option -> 'a option =
      fun f env x ->
        match x with
        | None -> x
        | Some y ->
          let z = f env y in
          if y == z then
            x
          else
            Some z
  end

class virtual ['e] monoid =
  object
    method virtual private zero : 'e

    method virtual private plus : 'e -> 'e -> 'e
  end

class virtual ['self] reduce_ty_base =
  object (self : 'self)
    inherit ['acc] monoid

    method private on_string : 'env. 'env -> string -> 'acc = (fun _ _ -> self#zero)

    method private on_int : 'env. 'env -> int -> 'acc = (fun _ _ -> self#zero)

    method private on_bool : 'env. 'env -> bool -> 'acc = (fun _ _ -> self#zero)

    method private on_symbol : 'env. 'env -> symbol -> 'acc = (fun _ _ -> self#zero)

    method private on_aloc : 'env. 'env -> ALoc.t -> 'acc = (fun _ _ -> self#zero)

    method private on_list : 'env 'a. ('env -> 'a -> 'acc) -> 'env -> 'a list -> 'acc =
      (fun f env xs -> self#list_fold_left f env self#zero xs)

    method private on_option : 'env 'a. ('env -> 'a -> 'acc) -> 'env -> 'a option -> 'acc =
      (fun f env -> Option.value_map ~default:self#zero ~f:(f env))

    method private list_fold_left
        : 'env 'a. ('env -> 'a -> 'acc) -> 'env -> 'acc -> 'a list -> 'acc =
      fun f env acc xs ->
        match xs with
        | [] -> acc
        | y :: ys ->
          let acc = self#plus acc (f env y) in
          self#list_fold_left f env acc ys
  end

class virtual ['self] mapreduce_ty_base =
  object (self : 'self)
    inherit ['acc] monoid

    method private on_string : 'env -> string -> string * 'acc = (fun _ x -> (x, self#zero))

    method private on_bool : 'env -> bool -> bool * 'acc = (fun _ x -> (x, self#zero))

    method private on_int : 'env -> int -> int * 'acc = (fun _ x -> (x, self#zero))

    method private on_symbol : 'env -> symbol -> symbol * 'acc = (fun _ x -> (x, self#zero))

    method private on_aloc : 'env -> ALoc.t -> ALoc.t * 'acc = (fun _ x -> (x, self#zero))

    method private on_list : 'a 'b. ('env -> 'a -> 'b * 'acc) -> 'env -> 'a list -> 'b list * 'acc
        =
      (fun f env -> self#list_fold_left f env ([], self#zero))

    method private on_option
        : 'a 'b. ('env -> 'a -> 'b * 'acc) -> 'env -> 'a option -> 'b option * 'acc =
      fun f env x ->
        match x with
        | None -> (None, self#zero)
        | Some x ->
          let (x', acc) = f env x in
          (Some x', acc)

    method private list_fold_left
        : 'a 'b. ('env -> 'a -> 'b * 'acc) -> 'env -> 'b list * 'acc -> 'a list -> 'b list * 'acc =
      fun f env acc xs ->
        match xs with
        | [] ->
          let (ys_rev, acc) = acc in
          (List.rev ys_rev, acc)
        | y :: ys ->
          let (acc_ys, acc) = acc in
          let (y', acc') = f env y in
          let acc'' = self#plus acc acc' in
          self#list_fold_left f env (y' :: acc_ys, acc'') ys
  end
