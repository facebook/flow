(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let rec mk_sig_tvar _cx _reason _resolved = failwith "TODO Annotation_inference.mk_sig_tvar"

and unresolved_tvar _cx _reason = failwith "TODO Annotation_inference.unresolved_tvar"

and resolve_id _cx _id _t = failwith "TODO Annotation_inference.resolve_id"

and flow _cx (_t1, _t2) = failwith "New_merge.flow"

and reposition _cx _loc _t = failwith "TODO Annotation_inference.reposition"

and specialize _cx _t _use_op _reason_op _reason_tapp _cache _ts =
  failwith "TODO Annotation_inference.specialize"

and mk_instance _cx _reason _t = failwith "TODO Annotation_inference.mk_instance"

and get_prop_internal _cx _use_op _loc _reason_op _propref _l =
  failwith "TODO Annotation_inference.get_prop"

and get_prop cx use_op loc reason name t = get_prop_internal cx use_op loc reason (reason, name) t

and get_elem _cx _use_op _reason ~key:_ _t = failwith "TODO Annotation_inference.get_elem"

and qualify_type cx use_op loc reason propref t = get_prop_internal cx use_op loc reason propref t

and mk_typeof_annotation _cx ?trace:_ _reason ?use_desc:_ ?internal:_ _t =
  failwith "TODO Annotation_inference.mk_typeof_annotation"

and assert_export_is_type _cx _reason _name _l =
  failwith "TODO Annotation_inference.assert_export_is_type"

and cjs_require _cx _l _reason _is_strict = failwith "TODO Annotation_inference.cjs_require"

and export_named _cx _reason _kind _named _t = failwith "TODO Annotation_inference.export_named"

and cjs_extract_named_exports _cx _reason _local_module _t =
  failwith "TODO Annotation_inference.cjs_extract_named_exports"

and import_typeof _cx _reason _export_t _export_name =
  failwith "TODO Annotation_inference.mk_import_type_of"

and import_default _cx _reason _import_kind _local_name _module_name _is_strict _l =
  failwith "TODO Annotation_inference.import_default"

and import_named _cx _reason _import_kind _export_name _module_name _is_strict _l =
  failwith "TODO Annotation_inference.import_named"

and import_ns _cx _reason _is_strict _l = failwith "TODO Annotation_inference.import_ns"

and copy_named_exports _cx ~from_ns:_ _reason ~module_t:_ =
  failwith "TODO Annotation_inference.copy_named_exports"

and copy_type_exports _cx ~from_ns:_ _reason ~module_t:_ =
  failwith "TODO Annotation_inference.copy_type_exports"

and unary_minus _cx _reason_op _l = failwith "TODO Annotation_inference.unary_minus"

and unary_not _cx _reason_op _l = failwith "TODO Annotation_inference.unary_not"

and mixin _cx _reason _l = failwith "TODO Annotation_inference.mixin"

and obj_rest _cx _reason _xs _t = failwith "TODO Annotation_inference.obj_rest"

and arr_rest _cx _use_op _reason _i _t = failwith "TODO Annotation_inference.arr_rest"

and object_spread _cx _use_op _reason _target _state _t =
  failwith "TODO Annotation_inference.object_spread"

and obj_test_proto _cx _reason_op _l = failwith "TODO Annotation_inference.obj_test_proto"

and existential _cx ~force:_ _reason = failwith "TODO Annotation_inference.existential"
