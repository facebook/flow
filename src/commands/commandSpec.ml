(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

exception Show_help
exception Failed_to_parse of string

module ArgSpec = struct
  type values_t = string list SMap.t

  type flag_arg_count =
  | No_Arg
  | Arg
  | Arg_List
  | Arg_Rest

  type 'a flag_t = {
    parse : string list option -> 'a;
    arg : flag_arg_count;
  }

  type flag_metadata = {
    doc : string;
    arg_count : flag_arg_count;
  }

  type ('a, 'b) t = {
    f : (values_t * 'a) -> (values_t * 'b);
    flags : flag_metadata SMap.t;
    anons : (string * string * flag_arg_count) list;
  }

  (* Partially applies [fn] with the values from [values]. Uses [spec] to
     figure out the order of the arguments and how to parse each value *)
  let apply spec values fn =
    let (values, main) = spec.f (values, fn) in
    main

  let apply_arg name arg_type f (values, main) =
    let (values, main) = f (values, main) in
    let value =
      try Some (SMap.find_unsafe name values : string list)
      with Not_found -> None
    in
    (values, main (arg_type.parse value))

  let pop_anon spec =
    match spec.anons with
    | [] -> (None, spec)
    | hd::tl -> (Some hd, {spec with anons = tl})

  let string = {
    parse = (function
    | Some [x] -> Some x
    | _ -> None
    );
    arg = Arg;
  }
  let bool = {
    parse = (function
    | Some ["0"]
    | Some ["false"]
    | None -> Some false
    | Some _ -> Some true
    );
    arg = Arg;
  }
  let int = {
    parse = (function
    | Some [x] -> Some (int_of_string x)
    | _ -> None
    );
    arg = Arg;
  }
  let enum values = {
    parse = (function
    | Some [x] ->
        if List.mem x values
        then Some x
        else raise (Failed_to_parse (Utils.spf
          "expected one of: %s"
          (String.concat ", " values)
        ))
    | _ -> None
    );
    arg = Arg;
  }

  let no_arg = {
    parse = (function
    | Some _ -> true
    | None -> false
    );
    arg = No_Arg;
  }

  let required arg_type = {
    parse = (function
    | None -> raise (Failed_to_parse "missing required arguments")
    | value -> match arg_type.parse value with
      | None -> raise (Failed_to_parse (Utils.spf
          "wrong type for required argument%s"
          (match value with Some [x] -> ": " ^ x | _ -> "")))
      | Some result -> result
    );
    arg = arg_type.arg;
  }

  let optional arg_type = {
    parse = (function
    | None -> None
    | value -> arg_type.parse value);
    arg = arg_type.arg;
  }

  let list_of arg_type = {
    parse = (function
      | None -> Some []
      | Some values ->
        Some (List.map (fun x ->
          match arg_type.parse (Some [x]) with
          | Some result -> result
          | None -> raise (Failed_to_parse (Utils.spf
              "wrong type for argument list item: %s" x))
        ) values)
    );
    arg = Arg_List;
  }

  let help_flag = SMap.empty |> SMap.add "--help" {
    doc = "This list of options";
    arg_count = No_Arg;
  }

  let apply_help (values, main) =
    let main help =
      if help then raise Show_help;
      main
    in
    apply_arg "--help" no_arg (fun x -> x) (values, main)

  (* Base spec, defines --help *)
  let empty = {
    f = apply_help;
    flags = help_flag;
    anons = [];
  }

  let flag name arg_type ~doc prev = {
    f = apply_arg name arg_type prev.f;
    flags = prev.flags |> SMap.add name {
      doc;
      arg_count = arg_type.arg;
    };
    anons = prev.anons;
  }

  let anon name arg_type ~doc prev = {
    f = apply_arg name arg_type prev.f;
    flags = prev.flags;
    anons = List.append prev.anons [(name, doc, arg_type.arg)];
  }

  let rest ~doc prev = {
    f = apply_arg "--" (optional (list_of string)) prev.f;
    flags = prev.flags;
    anons = List.append prev.anons [("--", doc, Arg_Rest)];
  }

  let dummy value prev = {
    f = (fun x -> let (values, main) = prev.f x in (values, main value));
    flags = prev.flags;
    anons = prev.anons;
  }

  let collect fn prev = {
    f = (fun x -> let (values, main) = prev.f x in (values, fn main));
    flags = prev.flags;
    anons = prev.anons;
  }
end

type ('a, 'b) builder_t = {
  name : string;
  doc : string;
  usage : string;
  args : ('a, 'b) ArgSpec.t;
}

type t = {
  cmdname : string;
  cmddoc : string;
  flags : ArgSpec.flag_metadata SMap.t;
  main : string list -> unit;
}

let no_dashes opt =
  if opt.[0] != '-' then opt
  else if opt.[1] != '-' then String.sub opt 1 ((String.length opt) - 1)
  else String.sub opt 2 ((String.length opt) - 2)

let is_arg arg = String.length arg > 1 && arg <> "--" && arg.[0] = '-'

let consume_args args =
  let is_done = ref false in
  List.partition
    (fun value ->
      (if not !is_done && is_arg value then is_done := true);
      not !is_done
    )
    args

let rec parse values spec = function
  | [] -> values
  | arg::args ->
      if is_arg arg
      then
        (* split "--foo=bar"::args into "--foo"::"bar"::args *)
        let arg, args = match (Str.bounded_split (Str.regexp "=") arg 2) with
        | arg::value::[] -> arg, value::args
        | arg::[] -> arg, args
        | _ -> assert false
        in
        parse_flag values spec arg args
      else parse_anon values spec arg args

and parse_flag values spec arg args =
  let flags = spec.ArgSpec.flags in
  try
    let flag = SMap.find_unsafe arg flags in
    match flag.ArgSpec.arg_count with
    | ArgSpec.No_Arg ->
      let values = SMap.add arg ["true"] values in
      parse values spec args

    | ArgSpec.Arg ->
      begin match args with
      | [] ->
        raise (Failed_to_parse (Utils.spf
          "option %s needs an argument."
          arg
        ))
      | value::args ->
        if is_arg value then
          raise (Failed_to_parse (Utils.spf
            "option %s needs an argument."
            arg
          ));
        let values = SMap.add arg [value] values in
        parse values spec args
      end

    | ArgSpec.Arg_List ->
      let (value_list, args) = consume_args args in
      let values = SMap.add arg value_list values in
      parse values spec args

    | ArgSpec.Arg_Rest -> failwith "Not supported"
  with
  | Not_found ->
    raise (Failed_to_parse (Utils.spf
      "unknown option '%s'."
      arg
    ))

and parse_anon values spec arg args =
  let (anon, spec) = ArgSpec.pop_anon spec in
  match anon with
  | Some (name, _, ArgSpec.Arg) ->
    let values = SMap.add name [arg] values in
    parse values spec args
  | Some (name, _, ArgSpec.Arg_List) ->
    let (value_list, args) = consume_args (arg::args) in
    let values = SMap.add name value_list values in
    parse values spec args
  | Some (name, _, ArgSpec.Arg_Rest) ->
    let values = SMap.add name args values in
    parse values spec []
  | Some (name, _, ArgSpec.No_Arg) ->
    assert false
  | None ->
    raise (Failed_to_parse (Utils.spf
      "unexpected argument '%s'."
      arg
    ))

let usage_string spec =
  let usage = spec.usage in
  let flags = SMap.fold (fun k v a -> (k, v)::a) spec.args.ArgSpec.flags [] in
  let cmp (a, _) (b, _) = String.compare (no_dashes a) (no_dashes b) in
  let flags = List.sort cmp flags in
  let col_width = flags |> List.fold_left (fun acc (a, _) ->
    max acc (String.length a)
  ) 0 in
  let flag_usage = flags
    |> List.map (fun (name, meta) ->
          Utils.spf "  %-*s  %s" col_width name meta.ArgSpec.doc
       )
    |> String.concat "\n"
  in
  (usage ^ "\n" ^ flag_usage)

let usage spec =
  print_endline (usage_string spec)

let main spec fn argv =
  try
    match argv with
    | cmd::subcmd::args when subcmd = spec.name ->
      let values = parse SMap.empty spec.args args in
      let main = ArgSpec.apply spec.args values fn in
      main ()
    | _ -> failwith "Missing subcommand"
  with
  | Show_help ->
      prerr_endline (usage_string spec);
      exit 0
  | Failed_to_parse msg ->
      prerr_endline (Utils.spf
        "%s: %s\n%s"
        (Filename.basename Sys.executable_name)
        msg
        (usage_string spec)
      );
      (* EX_USAGE -- command line usage error -- from glibc's sysexits.h *)
      exit 64

let raw_command spec main = {
  cmdname = spec.name;
  cmddoc = spec.doc;
  flags = spec.args.ArgSpec.flags;
  main;
}

let command spec main_ = raw_command spec (main spec main_)

let run command = command.main
let name command = command.cmdname
let doc command = command.cmddoc
let flags command = command.flags
