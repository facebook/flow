(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

module TLazyHeap = Typing_lazy_heap

type 'a command =
  | Rpc of 'a ServerRpc.t
  | Stream of streamed
  | Debug

and streamed =
  | SHOW of string
  | LIST_FILES
  | LIST_MODES
  | BUILD of ServerBuild.build_opts

(** Timeout on reading the command from the client - client probably frozen. *)
exception Read_command_timeout

(****************************************************************************)
(* Called by the client *)
(****************************************************************************)
let rpc : type a. Timeout.in_channel * out_channel -> a ServerRpc.t -> a
= fun (ic, oc) cmd ->
  Marshal.to_channel oc (Rpc cmd) [];
  flush oc;
  Timeout.input_value ic

let stream_request oc cmd =
  Marshal.to_channel oc (Stream cmd) [];
  flush oc

let connect_debug oc =
  Marshal.to_channel oc Debug [];
  flush oc

(****************************************************************************)
(* Called by the server *)
(****************************************************************************)
let stream_response (genv:ServerEnv.genv) env (ic, oc) ~cmd =
  match cmd with
  | LIST_FILES ->
      ServerEnv.list_files env oc;
      ServerUtils.shutdown_client (ic, oc)
  | LIST_MODES ->
      Relative_path.Map.iter env.ServerEnv.files_info begin fun fn fileinfo ->
        match Relative_path.prefix fn with
        | Relative_path.Root ->
          let mode = match fileinfo.FileInfo.file_mode with
            | None -> "php"
            | Some FileInfo.Mdecl -> "decl"
            | Some FileInfo.Mpartial -> "partial"
            | Some FileInfo.Mstrict -> "strict" in
          Printf.fprintf oc "%s\t%s\n" mode (Relative_path.to_absolute fn)
        | _ -> ()
      end;
      flush oc;
      ServerUtils.shutdown_client (ic, oc)
  | SHOW name ->
      output_string oc "starting\n";
      SharedMem.invalidate_caches();
      let qual_name = if name.[0] = '\\' then name else ("\\"^name) in
      output_string oc "class:\n";
      let class_name =
        match NamingGlobal.GEnv.type_canon_name qual_name with
        | None ->
          let () = output_string oc "Missing from naming env\n" in qual_name
        | Some canon ->
          let p = unsafe_opt @@ NamingGlobal.GEnv.type_pos canon in
          let () = output_string oc ((Pos.string (Pos.to_absolute p))^"\n") in
          canon
      in
      let class_ = TLazyHeap.get_class env.ServerEnv.tcopt class_name in
      (match class_ with
      | None -> output_string oc "Missing from typing env\n"
      | Some c ->
          let class_str = Typing_print.class_ env.ServerEnv.tcopt c in
          output_string oc (class_str^"\n")
      );
      output_string oc "\nfunction:\n";
      let fun_name =
        match NamingGlobal.GEnv.fun_canon_name qual_name with
        | None ->
          let () = output_string oc "Missing from naming env\n" in qual_name
        | Some canon ->
          let p = unsafe_opt @@ NamingGlobal.GEnv.fun_pos canon in
          let () = output_string oc ((Pos.string (Pos.to_absolute p))^"\n") in
          canon
      in
      let fun_ = TLazyHeap.get_fun env.ServerEnv.tcopt fun_name in
      (match fun_ with
      | None ->
          output_string oc "Missing from typing env\n"
      | Some f ->
          let fun_str = Typing_print.fun_ f in
          output_string oc (fun_str^"\n")
      );
      output_string oc "\nglobal const:\n";
      (match NamingGlobal.GEnv.gconst_pos qual_name with
      | Some p -> output_string oc (Pos.string (Pos.to_absolute p)^"\n")
      | None -> output_string oc "Missing from naming env\n");
      let gconst_ty = TLazyHeap.get_gconst env.ServerEnv.tcopt qual_name in
      (match gconst_ty with
      | None -> output_string oc "Missing from typing env\n"
      | Some gc ->
          let gconst_str = Typing_print.gconst gc in
          output_string oc ("ty: "^gconst_str^"\n")
      );
      output_string oc "typedef:\n";
      (match NamingGlobal.GEnv.typedef_pos qual_name with
      | Some p -> output_string oc (Pos.string (Pos.to_absolute p)^"\n")
      | None -> output_string oc "Missing from naming env\n");
      let tdef = TLazyHeap.get_typedef env.ServerEnv.tcopt qual_name in
      (match tdef with
      | None ->
          output_string oc "Missing from typing env\n"
      | Some td ->
          let td_str = Typing_print.typedef td in
          output_string oc (td_str^"\n")
      );
      flush oc;
      ServerUtils.shutdown_client (ic, oc)
  | BUILD build_opts ->
      let build_hook = BuildMain.go build_opts genv env oc in
      (match build_hook with
      | None -> ServerUtils.shutdown_client (ic, oc)
      | Some build_hook -> begin
        ServerTypeCheck.hook_after_parsing :=
          Some (fun genv old_env env updates ->
            (* subtle: an exception there (such as writing on a closed pipe)
             * will not be caught by handle_connection() because
             * we have already returned from handle_connection(), hence
             * this additional try.
             *)
            (try
              with_context
                ~enter:(fun () -> ())
                ~exit:(fun () -> ServerUtils.shutdown_client (ic, oc))
                ~do_:(fun () -> build_hook genv old_env env updates);
            with exn ->
              let msg = Printexc.to_string exn in
              Printf.printf "Exn in build_hook: %s" msg;
              EventLogger.master_exception msg;
            );
            ServerTypeCheck.hook_after_parsing := None
          )
      end)

let say_hello oc =
  output_string oc "Hello\n";
  flush oc

let read_client_msg ic =
  Timeout.with_timeout
    ~timeout:1
    ~on_timeout: (fun _ -> raise Read_command_timeout)
    ~do_: (fun timeout -> Timeout.input_value ~timeout ic)

let send_response_to_client (ic, oc) response =
  Marshal.to_channel oc response [];
  flush oc;
  ServerUtils.shutdown_client (ic, oc)

let handle genv env (ic, oc) =
  let msg = read_client_msg ic in
  match msg with
  | Rpc cmd ->
      let response = ServerRpc.handle genv env cmd in
      send_response_to_client (ic, oc) response;
      if cmd = ServerRpc.KILL then ServerUtils.die_nicely ()
  | Stream cmd -> stream_response genv env (ic, oc) ~cmd
  | Debug ->
    genv.ServerEnv.debug_channels <- Some (ic, oc);
    ServerDebug.say_hello genv
