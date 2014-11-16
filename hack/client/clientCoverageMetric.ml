open Hh_json
open Utils

module CL = Coverage_level

let result_to_json r = JAssoc [
  "counts", JAssoc (CL.CLMap.elements r.CL.counts |>
    List.map (fun (k, v) -> CL.string k, JInt v));
  "percentage", JFloat r.CL.percentage;
]

let rec entry_to_json = function
  | CL.Leaf r -> JAssoc [
      "type"   , JString "file";
      "result" , result_to_json r;
    ]
  | CL.Node (r, el) -> JAssoc [
      "type"     , JString "directory";
      "result"   , result_to_json r;
      "children" , JAssoc (SMap.elements (SMap.map entry_to_json el));
    ]

let to_json r_opt =
  let json = match r_opt with
  | Some e -> entry_to_json e
  | None -> JAssoc [ "internal_error", JBool true ]
  in json_to_string json
