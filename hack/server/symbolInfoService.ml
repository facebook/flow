(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

(* This module dumps all the symbol info(like fun-calls) in input files *)

type result = {
  fun_calls: SymbolFunCallService.result list;
  symbol_types: SymbolTypeService.result list;
}

let recheck_naming filename_l =
  List.iter filename_l begin fun file ->
    match Parser_heap.ParserHeap.get file with
    | Some ast -> begin
      let tcopt = TypecheckerOptions.permissive in
        Errors.ignore_ begin fun () ->
          (* We only need to name to find references to locals *)
          List.iter ast begin function
            | Ast.Fun f ->
                let _ = Naming.fun_ tcopt f in
                ()
            | Ast.Class c ->
                let _ = Naming.class_ tcopt c in
                ()
            | _ -> ()
          end
        end
      end
    | None -> () (* Do nothing if the file is not in parser heap *)
  end

let recheck_typing fileinfo_l =
  let tcopt = TypecheckerOptions.permissive in
  ignore(ServerIdeUtils.recheck tcopt fileinfo_l)

let helper acc filetuple_l =
  let fun_call_map = ref Pos.Map.empty in
  SymbolFunCallService.attach_hooks fun_call_map;
  let type_map = ref Pos.Map.empty in
  let lvar_map = ref Pos.Map.empty in
  SymbolTypeService.attach_hooks type_map lvar_map;
  let filename_l = List.rev_map filetuple_l fst in
  let fileinfo_l = List.rev_map filetuple_l snd in
  recheck_naming filename_l;
  recheck_typing fileinfo_l;
  SymbolFunCallService.detach_hooks ();
  SymbolTypeService.detach_hooks ();
  let fun_calls = Pos.Map.values !fun_call_map in
  let symbol_types = SymbolTypeService.generate_types !lvar_map !type_map in
  (fun_calls, symbol_types) :: acc

let parallel_helper workers filetuple_l =
  MultiWorker.call
    workers
    ~job:helper
    ~neutral:[]
    ~merge:List.rev_append
    ~next:(Bucket.make filetuple_l)

(* Format result from '(fun_calls * symbol_types) list' raw result into *)
(* 'fun_calls list, symbol_types list' and store in SymbolInfoService.result *)
let format_result raw_result =
  let result_list = List.fold_left raw_result ~f:begin fun acc bucket ->
    let result1, result2 = acc in
    let part1, part2 = bucket in
    (List.rev_append part1 result1,
    List.rev_append part2 result2)
  end ~init:([], []) in
  {
    fun_calls = fst result_list;
    symbol_types = snd result_list;
  }

(* Entry Point *)
let go workers file_list env =
  (* Convert 'string list' into 'fileinfo list' *)
  let filetuple_l = List.fold_left file_list ~f:begin fun acc file_path ->
    let fn = Relative_path.create Relative_path.Root file_path in
    match Relative_path.Map.get fn env.ServerEnv.files_info with
    | Some fileinfo -> (fn, fileinfo) :: acc
    | None -> acc
  end ~init:[]
  in
  let raw_result =
    if (List.length file_list) < 10 then
      helper [] filetuple_l
    else
      parallel_helper workers filetuple_l in
  format_result raw_result
