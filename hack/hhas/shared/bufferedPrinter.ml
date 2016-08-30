let rec list ?(sep=", ") f o l =
  match l with
  | [] -> ()
  | [x] -> f o x
  | x :: rl -> f o x; o#out sep; list ~sep f o rl

let name o (_, x) = o#out x

class out buf = object(self)
  val mutable margin: int = 0
  val mutable last_was_nl: bool = true

  method private pad =
    if last_was_nl then
      for i = 1 to margin do
        Buffer.add_char buf ' '
      done;
    last_was_nl <- false;

  method out x =
    self#pad;
    Buffer.add_string buf x

  method out_char c =
    self#pad;
    Buffer.add_char buf c

  method newline =
    last_was_nl <- true;
    Buffer.add_char buf '\n'

  method space =
    last_was_nl <- false;
    Buffer.add_char buf ' '

  method margin f =
    margin <- margin + 2;
    let () = f() in
    margin <- margin - 2;
    ()

  method contents () =
    Buffer.contents buf

end

let new_out () =
  let buf = Buffer.create 256 in
  new out buf

let pp f ast =
  let o = new_out () in
  let _ = f o ast in
  let str = o#contents () in
  output_string stdout str;
  print_newline ()

let out_to_file o file =
  let oc = open_out file in
  Printf.fprintf oc "%s\n" (o#contents ());
  close_out oc;
