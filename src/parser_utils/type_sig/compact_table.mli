(* (c) Facebook, Inc. and its affiliates. Confidential and proprietary. *)

(* This module is useful for converting an OCaml data structure into a compact,
 * table-based representation. This module supports cycles and ensures the
 * resulting tables preserve insertion order.
 *
 * For example, we use this to create a compact representation of file
 * signatures, where each named definition is stored in source order in an array
 * and each reference is an offset into that array.
 *
 * The way you use this data structure is similar to a copying collector, but
 * instead of copying data from an old space to a new space, we are copying from
 * one representation to another representation, where the new representation
 * may only contain parts of the original.
 *
 * Usage of this data structure can be broken down into phases:
 *
 * 1. Construction
 *
 * For each "table" in the compact output, create a builder. Use that builder to
 * construct nodes, which can then be stored in the data structure you will
 * eventually compact.
 *
 * The order of insertion will be preserved in the final compacted table, so if
 * you push A and then push B, the final table will be [A, B].
 *
 * If your data structure contains a cycle, any back edges must be stored within
 * a node, as nodes contain the machinery to detect and handle cycles.
 *
 * For example, in the signature parsing phase, the scope is a mapping from
 * names to nodes. Resolved references to those names are also nodes which have
 * been looked up from the scope.
 *
 * 2. Marking
 *
 * Only nodes which have been marked will appear in the compacted table. To mark
 * nodes, visit whatever data structure you are using to store the nodes,
 * calling `mark` on each visited node.
 *
 * For example, in signature parsing we only mark nodes which are reachable from
 * the exports of the file. Any nodes which are not reachable correspond to
 * definitions which can be excluded from the signature entirely.
 *
 * 3. Compaction
 *
 * The `compact` function returns a new value which elides unmarked nodes
 * entirely. Furthermore, any marked nodes are now imbued with an index that
 * represents the 0-based offset into the table, which can be retrieved using
 * the `index_exn` function.
 *
 * Note that the table has not actually been created at this point. Rather,
 * we've only calculated the index of where the data will be, once copied.
 *
 * Once compacted, the builder is "consumed" and reverts to its initial state.
 * The builder can be re-used to build a new table starting at phase 1.
 *
 * 4. Copy
 *
 * The `copy` function will allocate an array populated with data. This is the
 * fully compacted table. The provided transform function will be applied to
 * each marked node, in order, to produce the value stored at each index. *)

module Make () : sig
  type 'a builder

  type 'a indexed

  type 'a t

  type index = private int [@@deriving show]

  type 'a node

  val create : unit -> 'a builder

  val push : 'a builder -> 'a -> 'a node

  val tail_exn : 'a builder -> 'a node

  val splice : 'a node -> ('a builder -> 'b) -> 'b

  val modify : 'a node -> ('a -> 'a) -> unit

  (* If this node has already been marked, does nothing. Otherwise calls the
   * provided function with the node's value. *)
  val mark : 'a node -> ('a -> unit) -> unit

  val compact : ?merge:('a -> 'a -> 'a option) -> 'a builder -> 'a indexed

  (* This function is guaranteed to succeed if, and only if, the node has
   * already been marked and the builder producing this node was compacted. *)
  val index_exn : 'a node -> index

  val copy : ('a -> 'b) -> 'a indexed -> 'b t

  val get : 'a t -> index -> 'a

  val length : 'a t -> int

  val iter : ('a -> unit) -> 'a t -> unit

  val iteri : (index -> 'a -> unit) -> 'a t -> unit

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (index -> 'a -> 'b) -> 'a t -> 'b t

  val to_array : 'a t -> 'a array

  val heap_size : ('a -> Type_sig_heap.size) -> 'a t -> Type_sig_heap.size

  val to_heap :
    (Type_sig_heap.chunk -> 'a -> 'k Type_sig_heap.addr) ->
    Type_sig_heap.chunk ->
    'a t ->
    'k Type_sig_heap.addr_map Type_sig_heap.addr

  val from_heap :
    (Type_sig_heap.heap -> 'k Type_sig_heap.addr -> 'a) ->
    Type_sig_heap.heap ->
    'k Type_sig_heap.addr_map Type_sig_heap.addr ->
    'a t
end
