(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FlowJS : Type_annotation_sig.ConsGen

module Annot : Type_annotation_sig.ConsGen

module Make (_ : Type_annotation_sig.ConsGen) (_ : Statement_sig.S) : Type_annotation_sig.S
