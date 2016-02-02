(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast
open Hh_match_utils
module Up = Unparser.Unparse

(* NOTE: Will only work if the list of patches is constructed off of the
   exact same AST that is passed to the matcher *)
(* NOTE cannot use a single comment for patches because comments do
   not have correct line numbers *)

(* used in indexing stmts, exprs in the pattern *)
(* Assumption made here: not many transformations are being made, pattern
   is decently short, so assoc lists are okay. Additionally maps prohibit
   me from checking pointer equality, and structural equality is O(N) worst
   case on stmts. As a result assoc lists are faster than maps. *)
type pat_acc = {
    (* map from stmt to the index of the line num *)
    s_line_map : (stmt * (int * int)) list;
    e_line_map : (expr * (int * int)) list;
    (* so that we only apply patches to the outermost AST node *)
    stmt_covering_map : (stmt * stmt list) list;
    expr_covering_map : (expr * expr list) list;
  }

(* RANGE ADJUSTMENT FUNCTIONS *)
(* helpers *)
let rewind_beg_loc_stmt src s_start =
  let whitespace_re = Str.regexp "\n[ \t]*" in
  try
    (* Goto last newline, add 1 *)
    let start = 1 + Str.search_backward
          whitespace_re src s_start in
    (* all the weird if checks are to make sure the match we found starts
       in the right place *)
    let end_ = Str.match_end () in
    if end_ <> s_start
    then s_start
    else start
  with Not_found -> s_start

(* skips over a semicolon if it exists and a newline if it exists, returning
   if it skipped a newline *)
let advance_end_loc_stmt src s_end =
  let semicolon_re = Str.regexp "[ \t\r\n]*;" in
  let newline_re = Str.regexp "[ \t\r]*\n" in
  let sc_res =
    try
      let res_start = Str.search_forward semicolon_re src s_end in
      if res_start <> s_end
      then s_end
      else Str.match_end ()
    with Not_found -> s_end in
  try
    let res_start = Str.search_forward newline_re src sc_res in
    if res_start <> sc_res
    then sc_res, false
    else Str.match_end (), true
  with Not_found -> sc_res, false

(* TODO fix leading/trailing commas in all cases for deleting
        expressions in a list *)
let rewind_beg_loc_expr src e_start =
  let comma_re = Str.regexp "[ \t\r\n]*," in
  let whitespace_re = Str.regexp "[ \t\r\n]*" in
  try
    let whitespace_start =
      Str.search_backward
        whitespace_re src e_start in
    let white_end = Str.match_end () in
    if white_end <> e_start
    then e_start
    else
      try
        let res = Str.search_backward
          comma_re src whitespace_start in
        let comma_end = Str.match_end () in
        if comma_end <> whitespace_start
        then whitespace_start
        else res
      with Not_found -> whitespace_start
  with Not_found -> e_start

let advance_end_loc_expr src e_end =
  let comma_re = Str.regexp "[ \t\r\n]*," in
  let whitespace_re = Str.regexp "[ \t\r\n]*" in
  try
    let res_start = Str.search_forward comma_re src e_end in
    if res_start <> e_end
    then e_end, false
    else
      let c_end = Str.match_end () in
      try
        let res_start = Str.search_forward whitespace_re src c_end in
        if res_start <> c_end
        then c_end, false
        else
          Str.match_end (), true
      with Not_found -> c_end, false
  with Not_found -> e_end, false

(* actual functions *)
let adjust_range_stmt src s_start s_end =
  let new_end, skipped_newline = advance_end_loc_stmt src s_end in
  if skipped_newline
  then rewind_beg_loc_stmt src s_start, new_end
  else s_start, new_end

(* will remove either a leading or trailing comma if they exist *)
let adjust_range_expr src e_start e_end =
  let new_end, skipped_comma = advance_end_loc_expr src e_end in
  if skipped_comma
  then e_start, new_end
  else rewind_beg_loc_expr src e_start, e_end

let dummy_adjuster _ n_start n_end =
  n_start, n_end

(* end RANGE ADJUSTERS*)

(* PREPROCESSING PATCH FILES *)

type patch_cmd =
  | DeleteStmt
  | DeleteExpr
  | Insert
  | ReplaceStmt
  | ReplaceExpr
  | NoAction

(* default values *)
let default_pat_acc =
  { s_line_map = []; e_line_map = [];
    stmt_covering_map = []; expr_covering_map = [] }

let find_substring to_search substring =
  let re = Str.regexp_string substring in
  (* ignore the position of the match because we don't care*)
  try Str.search_forward re to_search 0
  with Not_found -> -1

let command_of_comment (comment : string) =
  let sdelete_action = function _ -> DeleteStmt in
  let edelete_action = function _ -> DeleteExpr in
  let insert_action = function _ -> Insert in
  let sreplace_action = function _ -> ReplaceStmt in
  let ereplace_action = function _ -> ReplaceExpr in
  let actions =
    ["DELETE STMT", sdelete_action;
     "DELETE EXPR", edelete_action;
     "INSERT", insert_action;
     "REPLACE EXPR", ereplace_action;
     "REPLACE STMT", sreplace_action;] in
  let rec check_keywords = function
    (* default to normal identifier *)
    | [] -> NoAction
    | (keyword, action) :: tail ->
       let substr_pos = find_substring comment keyword in
       if substr_pos <> -1
       then (action substr_pos)
       else check_keywords tail in
  check_keywords actions

(* builds the necessary list of indices over the pattern. All lists in
   the accumulator should be in reversed pre-order traversal order *)
class line_numbering_visitor (file : Relative_path.t) (source : string) =
object
  inherit [pat_acc] Ast_visitor.ast_visitor as super

  method! on_stmt acc stmt =
    begin
      let start_p, end_p =
        Ast_code_extent.source_extent_stmt file source stmt in
      let self_res = stmt, (File_pos.line start_p, File_pos.line end_p) in
      let acc = { acc with s_line_map = self_res :: acc.s_line_map } in
      let child_acc = super#on_stmt default_pat_acc stmt in
      let covered_nodes = List.map fst child_acc.s_line_map in
      { s_line_map = child_acc.s_line_map @ acc.s_line_map;
        e_line_map = child_acc.e_line_map @ acc.e_line_map;
        stmt_covering_map =
          child_acc.stmt_covering_map @
            (stmt, covered_nodes) :: acc.stmt_covering_map;
        expr_covering_map =
          child_acc.expr_covering_map @ acc.expr_covering_map }
    end

  method! on_expr acc expr =
    begin
      let start_p, end_p =
        Ast_code_extent.source_extent_expr file source expr in
      let self_res = expr, (File_pos.line start_p, File_pos.line end_p) in
      let acc = { acc with e_line_map = self_res :: acc.e_line_map } in
      let child_acc = super#on_expr default_pat_acc expr in
      let covered_nodes = List.map fst child_acc.e_line_map in
      { s_line_map = child_acc.s_line_map @ acc.s_line_map;
        e_line_map = child_acc.e_line_map @ acc.e_line_map;
        stmt_covering_map =
          child_acc.stmt_covering_map @ acc.stmt_covering_map;
        expr_covering_map =
          child_acc.expr_covering_map @
            (expr, covered_nodes) :: acc.expr_covering_map }
    end
end

(* given the pattern and target file, content, source, get the map of
   transformations that need to be done *)
(* TODO add ability to make additions or replacements *)
let preprocess_patch_file
      (p_file : Relative_path.t)
      (p_source : string)
      (p_parser_ret : Parser_hack.parser_return)
      (t_file : Relative_path.t)
      (t_source : string)
      (t_parser_ret : Parser_hack.parser_return) : patch_maps =
  let plnv = new line_numbering_visitor p_file p_source in
  let p_program = p_parser_ret.Parser_hack.ast in
  let p_numbered_ast = plnv#on_program default_pat_acc p_program in
  let p_numbered_ast =
    { p_numbered_ast with
      s_line_map = List.rev p_numbered_ast.s_line_map;
      e_line_map = List.rev p_numbered_ast.e_line_map } in
  (* some reason they're reversed *)
  let p_comments = List.rev p_parser_ret.Parser_hack.comments in
  (* do same operations for the target pattern. will be useful once we
     are able to do add and replace *)
  let tlnv = new line_numbering_visitor t_file t_source in
  let t_program = t_parser_ret.Parser_hack.ast in
  let t_numbered_ast = tlnv#on_program default_pat_acc t_program in
  let t_numbered_ast =
    { t_numbered_ast with
      s_line_map = List.rev t_numbered_ast.s_line_map;
      e_line_map = List.rev t_numbered_ast.e_line_map } in
  (* some reason they're reversed *)
  let t_comments = List.rev t_parser_ret.Parser_hack.comments in
  (* finds the AST nodes (stmts or exprs) that span the specified line range.
     will find nodes higher in the AST first and not include their children
     in the result:
     e.g. if a line range corresponds to an if stmt we will return just the if
     stmt and not its child nodes *)
  let contained_elems
        (type a)
        (elem_list : (a * (int * int)) list)
        (covering_map : (a * a list) list)
        (start_l : int)
        (end_l : int) : a list =
    List.fold_left
      (fun acc elem ->
       (* blacklist is to make sure we don't find the children nodes *)
       let contained, blacklist = acc in
       let el, (beg_line, end_line) = elem in
       if beg_line >= start_l && end_line <= end_l &&
            not (List.exists (fun bl_el -> bl_el == el) blacklist)
       then
         let covers = List.assq el covering_map in
         (el :: contained), covers @ blacklist
       else acc)
      ([], [])
      elem_list
    |> fst |> List.rev in
  (* process comments (commands) in the pattern and target *)
  let rec proc_comments
            (p_comments : (Pos.t * string) list)
            (t_comments : (Pos.t * string) list): patch_maps =
    match t_comments with
    | [] -> { Hh_match_utils.stmt_delete_list = [];
              Hh_match_utils.expr_delete_list = [];
              Hh_match_utils.stmt_transf_map = [];
              Hh_match_utils.expr_transf_map = [] }
    | hd :: tl ->
       let start_l = Pos.line (fst hd) in
       (* next comment delimits the end of this transformation *)
       let end_l, tl =
         match tl with
         | [] ->
            failwith "Unmatched comment, comments must
                      come in pairs to delimit a range"
         | hd :: tl -> Pos.line (fst hd), tl in
       (* figure out what the comment is telling us to do *)
       let command = command_of_comment (snd hd) in
       match command with
       | DeleteExpr ->
          (* find the elements of the pattern that are being deleted *)
          let exprs =
            contained_elems
              p_numbered_ast.e_line_map
              p_numbered_ast.expr_covering_map
              start_l
              end_l in
          let rest_res = proc_comments p_comments tl in
          { rest_res with expr_delete_list = exprs @ rest_res.expr_delete_list }
       | DeleteStmt ->
          (* find the elements of the pattern that are being deleted *)
          let stmts =
            contained_elems
              p_numbered_ast.s_line_map
              p_numbered_ast.stmt_covering_map
              start_l
              end_l in
          let rest_res = proc_comments p_comments tl in
          { rest_res with stmt_delete_list = stmts @ rest_res.stmt_delete_list }
       | ReplaceStmt -> begin
          let get_elems numbered_ast =
            contained_elems
              numbered_ast.s_line_map
              numbered_ast.stmt_covering_map
              start_l
              end_l in
          let stmts = get_elems p_numbered_ast in
          let tgts = get_elems t_numbered_ast in
          match stmts with
          | stmt :: [] ->
             let rest_res = proc_comments p_comments tl in
             { rest_res with
               stmt_transf_map = (stmt, tgts) :: rest_res.stmt_transf_map }
               (* do we want to fail here? *)
          | _ -> failwith "invalid replace specification" end
       | ReplaceExpr -> begin
          let get_elems numbered_ast =
            contained_elems
              numbered_ast.e_line_map
              numbered_ast.expr_covering_map
              start_l
              end_l in
          let exprs = get_elems p_numbered_ast in
          let tgts = get_elems t_numbered_ast in
          match exprs with
          | expr :: [] ->
             let rest_res = proc_comments p_comments tl in
             { rest_res with
               expr_transf_map = (expr, tgts) :: rest_res.expr_transf_map }
          | _ -> failwith "Invalid replace specification" end
       | _ -> proc_comments p_comments tl in
  proc_comments p_comments t_comments

let to_string_patch_maps
      (maps : patch_maps)
      (p_file : Relative_path.t)
      (p_content : string) : string list * string list =
  let to_string_transf
        (type a)
        (src_ext_fn :
           Relative_path.t -> string -> a -> File_pos.t * File_pos.t)
        (transformation : a) =
      let src_ext_before =
        src_ext_fn p_file p_content transformation in
      let strn_from_ext ext content =
        (Ast_code_extent.format_file_pos (fst ext)) ^
          " " ^
            (Ast_code_extent.format_file_pos (snd ext)) ^ ":\n" ^
              if File_pos.is_dummy (fst ext) || File_pos.is_dummy (snd ext)
              then "No source extent available"
              else Ast_code_extent.lexing_slice_to_string ext content in
      let pattern_strn = strn_from_ext src_ext_before p_content in
      "Deleting: \n" ^ pattern_strn in
  let stmt_strns =
    List.map
      (to_string_transf Ast_code_extent.source_extent_stmt)
      maps.stmt_delete_list in
  let expr_strns =
    List.map
      (to_string_transf Ast_code_extent.source_extent_expr)
      maps.expr_delete_list in
  stmt_strns, expr_strns

(* END PREPROCESSING PATCH FILES *)

(* GENERAL PATCHING FNS *)

(* For creating patches from node types that are not stmt, expr *)
let create_any_patch
      ~(extent : File_pos.t * File_pos.t)
      ~(target_string : string) : patch option =
  if File_pos.is_dummy (fst extent) ||
     File_pos.is_dummy (snd extent)
  then None
  else
  let start_loc = File_pos.offset (fst extent) in
  let end_loc = File_pos.offset (snd extent) in
  Some { start_loc; end_loc; result_str = target_string;
         range_adjustment_fn = dummy_adjuster }

let create_patch
      ~(adjust_ranges : bool)
      ~(src : string)
      ~(src_node : ast_node)
      ~(tgt_node : ast_node) : patch option =
  let dummy_filepath = Relative_path.create Relative_path.Dummy "/" in
  let tgt_unparsed, src_pos, range_adjustment_fn =
    match tgt_node, src_node with
    | Hh_match_utils.Expr t, Hh_match_utils.Expr s ->
       Up.u_expr t,
       Ast_code_extent.source_extent_expr dummy_filepath src s,
       adjust_range_expr
    | Hh_match_utils.Stmt t, Hh_match_utils.Stmt s ->
       Up.u_stmt t,
       Ast_code_extent.source_extent_stmt dummy_filepath src s,
       adjust_range_stmt
    | _ -> failwith "not implemented" in
  let raw_tgt_string = Unparsed.to_string tgt_unparsed in
  if File_pos.is_dummy (fst src_pos) ||
     File_pos.is_dummy (snd src_pos)
  then None
  else
    let range_adjustment_fn =
      if adjust_ranges
      then range_adjustment_fn
      else dummy_adjuster in
    let start_loc = File_pos.offset (fst src_pos) in
    let end_loc = File_pos.offset (snd src_pos) in
    Some { start_loc; end_loc;
           result_str = raw_tgt_string;
           range_adjustment_fn }

let apply_patches
      ~(format_result : bool)
      ~(src : string)
      ~(patches : patch list) : string =
  (* check the rest of the list for a duplicate or overlap, will usually
     find one quickly, list of patches is short so inefficiency is fine *)
  let rec exists_overlapping_patch
            (apatch : patch) (to_search : patch list) : bool =
    match to_search with
    | [] -> false
    | hd :: tl ->
       (* do this to short circuit computation early *)
       if (apatch.start_loc < hd.end_loc &&
             apatch.start_loc >= hd.start_loc) ||
            (apatch.end_loc <= hd.end_loc &&
               apatch.end_loc > hd.start_loc)
       then true
       else exists_overlapping_patch apatch tl in

  (* Need to remove any overlapping patches.
     Pre: patches covering a larger range occur later in the list *)
  let rec remove_overlapping_patches (patches : patch list) =
    match patches with
    | [] -> []
    | hd :: tl ->
       if exists_overlapping_patch hd tl
       then remove_overlapping_patches tl
       else hd :: remove_overlapping_patches tl in

  (* Combines overlapping ranges into single larger ranges
     Pre: ranges sorted in order of increasing start position *)
  let rec combine_format_ranges
            (ranges : (int * int) list) : (int * int) list =
    match ranges with
    | [] -> []
    | [x] -> [x]
    | r1 :: r2 :: tl ->
       if snd r1 >= fst r2
       then combine_format_ranges ((fst r1, snd r2) :: tl)
       else r1 :: r2 :: combine_format_ranges tl in

  (* Expands a range to the nearest newlines if possible *)
  let expand_to_containing_newlines
        (src : string) (p_range : int * int) : int * int =
    let p_start, p_end = p_range in
    let new_start =
      if p_start < 0 || p_start >= String.length src
      then
        failwith ("Invalid index for expanding " ^
                    (Printf.sprintf "%d" p_start))
      else
        try
          Str.search_backward (Str.regexp "\n") src p_start
        with Not_found -> fst p_range in
    let new_end =
      try
        let _ = Str.search_forward (Str.regexp ".*\n") src p_end in
        Str.match_end ()
      with Not_found -> snd p_range in
    new_start, new_end in

  (* applies a sorted list of patches, returns new source and the char
     position ranges of where the patches are (so that we can run hh_format
     over them correctly *)
  let rec apply
            (src : string)
            (* position in original source for taking substrings *)
            (pos_in_src : int)
            (* position in new source for finding where patches went *)
            (patch_pos : int)
            (patches : patch list) : string * (int * int) list =
    match patches with
    | [] ->
       (* return the rest of the source file *)
       let cutlen = String.length src - pos_in_src in
       if cutlen > 0
       then String.sub src pos_in_src cutlen, []
       else "", []
    | hd :: tl ->
       (* split the source file into before and after the patch, inserting
          the patch in the middle, recursing on the part after the patch *)
       let bef_patch_len = hd.start_loc - pos_in_src in
       let before, p_start_loc =
         (* in case there are two adjacent patches *)
         if bef_patch_len <= 0
         then "", patch_pos
         else
           String.sub
             src pos_in_src bef_patch_len, patch_pos + bef_patch_len in
       let p_end_loc = p_start_loc + String.length hd.result_str in
       let tl_string, tl_patches = apply src hd.end_loc p_end_loc tl in
       (* the - 1 is necessary because patch_pos is the character after the
          end of the last patch *)
       let bef_patch_loc = if p_start_loc = 0 then 0 else p_start_loc - 1 in
       (before ^ hd.result_str ^ tl_string),
       (bef_patch_loc, p_end_loc) :: tl_patches in

  let modes = [Some FileInfo.Mstrict; Some FileInfo.Mpartial] in
  (* Creates patches that will correctly format the source given locations of
     patches that occured
     Pre: ranges do not overlap and are in order from closest to beginning of
          file to closest to end of file *)
  let rec format_new_src
            (new_src : string)
            (patch_locs : (int * int) list) : patch list =
    match patch_locs with
    | [] -> []
    | curpatch :: otherpatches ->
       let f_start, f_end = curpatch in
       (* f_end + 1 because hh_format runs on a weird interval *)
       let format_errs, format_res =
         Errors.do_
           (fun () ->
            Format_hack.region
              modes Path.dummy_path f_start (f_end + 1) new_src) in
       let after_format_str =
         match format_res with
         | Format_hack.Success res -> res
         | _ ->
            (* Make readable text out of hh_format's error then throw our own *)
            let errors =
              (List.fold_left
                 (fun so_far str_pair ->
                  so_far ^ "\n" ^ fst str_pair ^ " " ^ snd str_pair)
                 ""
                 (Common_exns.flatten_error
                    (Common_exns.ParseErrors format_errs))) in
            failwith ("Failed to format file:\n" ^ errors) in
       let format_patch =
         { start_loc = f_start;
           end_loc = f_end;
           result_str = after_format_str;
           range_adjustment_fn = dummy_adjuster } in
       format_patch :: format_new_src new_src otherpatches in

     (* Take the patches from the matcher and remove overlapping ones, then sort
        them to make sure they're applied in the correct order *)
     let to_apply =
       patches |>
         List.map (fun patch ->
                   (* Deal with commas, semicolons and newlines *)
                   let start_loc, end_loc =
                     patch.range_adjustment_fn
                       src patch.start_loc patch.end_loc in
                   { patch with start_loc; end_loc }) |>
         (* Sorts by size so that larger patches occur later *)
         List.sort (fun p1 p2 ->
                    (p1.end_loc - p1.start_loc) -
                      (p2.end_loc - p2.start_loc)) |>
         remove_overlapping_patches |>
         (* Sort such that patches occur in the order they should be applied *)
         List.sort (fun p1 p2 -> p1.start_loc - p2.start_loc) in

     let new_src, patch_locs = apply src 0 0 to_apply in
     if not format_result
     then new_src
     else
       (* Now we want to format the result, we start by changing the ranges to
          give hh_format a little larger range than just the patch itself. *)
       let format_ranges =
         List.map (expand_to_containing_newlines new_src) patch_locs in
       (* Now we want to combine overlapping ranges *)
       let combined_format_ranges = combine_format_ranges format_ranges in
       (* Find and apply patches based off hh_format *)
       let format_patches = format_new_src new_src combined_format_ranges in
       (* Some edge cases require this *)
       let format_patches_toapply =
         format_patches |>
           List.sort (fun p1 p2 ->
                      (p1.end_loc - p1.start_loc) -
                        (p2.end_loc - p2.start_loc)) |>
           remove_overlapping_patches |>
           List.sort (fun p1 p2 -> p1.start_loc - p2.start_loc) in
       let final_src, _ = apply new_src 0 0 format_patches_toapply in
       final_src
