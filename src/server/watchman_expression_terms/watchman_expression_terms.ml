(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let make ~options =
  let module J = Hh_json_helpers.AdhocJsonHelpers in
  let file_options = Options.file_options options in
  let suffixes =
    let exts = SSet.elements @@ Files.get_all_watched_extensions file_options in
    let exts = Files.flow_ext :: exts in
    exts
    (* Turn .foo.bar into .bar, since suffix can't deal with multi-part extensions *)
    |> List.map (fun ext -> Filename.extension ("foo" ^ ext))
    (* Strip off the leading '.' *)
    |> List.map (fun ext ->
           if ext <> "" && ext.[0] = '.' then
             String.sub ext 1 (String.length ext - 1)
           else
             ext)
  in
  (* Unfortunately watchman can't deal with absolute paths. Its "wholename" feature only
   * works for relative paths to the watch root, and we don't know the watch root until we
   * init.
   *
   * Luckily, all we really need is to specify a superset of the files we care about. So
   * watching all .flowconfigs instead of just our .flowconfig is fine *)
  let absolute_paths =
    (* Config file *)
    let flowconfig_name = Options.flowconfig_name options in
    [Server_files_js.config_file flowconfig_name @@ Options.root options]
  in
  (* Include any file with this basename *)
  let basenames = "package.json" :: List.map Filename.basename absolute_paths in
  [
    J.strlist ["type"; "f"];
    (* Watch for files *)
      J.pred "anyof" @@ [J.assoc_strlist "suffix" suffixes; J.assoc_strlist "name" basenames];
    J.pred "not"
    @@ [
         (* Ignore changes in source control dirs *)
           J.pred "anyof"
           @@ [
                J.strlist ["dirname"; ".hg"];
                J.strlist ["dirname"; ".git"];
                J.strlist ["dirname"; ".svn"];
              ];
       ];
  ]
