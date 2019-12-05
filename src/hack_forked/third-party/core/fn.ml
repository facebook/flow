let const c =
  ();
  (fun _ -> c)

let non f =
  ();
  (fun x -> not (f x))

let forever f =
  let rec forever () =
    f ();
    forever ()
  in
  (try forever () with e -> e)

(* The typical use case for these functions is to pass in functional arguments and get
   functions as a result. For this reason, we tell the compiler where to insert
   breakpoints in the argument-passing scheme. *)
let compose f g =
  ();
  (fun x -> f (g x))

let flip f =
  ();
  (fun x y -> f y x)

let rec apply_n_times ~n f x =
  if n <= 0 then
    x
  else
    apply_n_times ~n:(n - 1) f (f x)
