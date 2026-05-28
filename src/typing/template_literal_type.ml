(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason

(* Extract string representations from a literal type.
   Returns None if the type is not a concrete literal (or a union of concrete literals).
   Generics are intentionally not expanded to their bound when [expand_generics] is
   false: doing so would erase the dependency on the type parameter and break
   inference for APIs like `<T extends 'a' | 'b'>(x: \`get${T}\`): T`. Pass
   [~expand_generics:true] when the goal is to enumerate the strings a template
   could ever produce. *)
let rec extract_strings_aux ~expand_generics = function
  | DefT (_, SingletonStrT { value = OrdinaryName s; _ }) -> Some [s]
  | DefT (_, SingletonNumT { value = (f, _); _ }) -> Some [Dtoa.ecma_string_of_float f]
  | DefT (_, SingletonBoolT { value = b; _ }) -> Some [string_of_bool b]
  | DefT (_, NullT) -> Some ["null"]
  | DefT (_, VoidT) -> Some ["undefined"]
  | DefT (_, SingletonBigIntT { value = (bigint, raw); _ }) ->
    (* Stringify to the decimal form JavaScript's `${bigintValue}` produces at
       runtime. For values that fit in Int64 we use Int64.to_string. Otherwise,
       we can only safely produce a decimal string when the raw literal is
       itself decimal — for hex/oct/bin raw forms that overflow Int64, decimal
       conversion would require bignum arithmetic that's not in stdlib. In that
       case, return None so the template literal remains unresolved (callers
       fall back to TemplateLiteralT, which is conservative but correct). *)
    (match bigint with
    | Some v -> Some [Int64.to_string v]
    | None ->
      let stripped = String.sub raw 0 (String.length raw - 1) in
      let start =
        if String.length stripped > 0 && stripped.[0] = '-' then
          1
        else
          0
      in
      let is_non_decimal_radix =
        start + 1 < String.length stripped
        && stripped.[start] = '0'
        &&
        match stripped.[start + 1] with
        | 'x'
        | 'X'
        | 'o'
        | 'O'
        | 'b'
        | 'B' ->
          true
        | _ -> false
      in
      if is_non_decimal_radix then
        None
      else
        Some [stripped |> String.split_on_char '_' |> String.concat ""])
  | UnionT (_, rep) ->
    Base.List.map (UnionRep.members rep) ~f:(extract_strings_aux ~expand_generics)
    |> Base.Option.all
    |> Base.Option.map ~f:List.concat
  | GenericT { bound; _ } when expand_generics -> extract_strings_aux ~expand_generics bound
  (* Nested template literal whose placeholders are themselves concrete (or
     unions of concrete singletons): recursively extract each placeholder's
     producible strings and take the Cartesian product against the quasis.
     This is the recursive case that lets `` `a.${Join<U,D>}` `` collapse
     once `Join<U,D>` evaluates to a literal during conditional-type unrolling. *)
  | TemplateLiteralT { quasis; types; _ } ->
    let extracted = Base.List.map types ~f:(extract_strings_aux ~expand_generics) in
    (match Base.Option.all extracted with
    | None -> None
    | Some string_lists ->
      let rec product prefix quasis string_lists =
        match (quasis, string_lists) with
        | ([q], []) -> Some [prefix ^ q]
        | (q :: qs, ss :: rest) ->
          Base.List.fold ss ~init:(Some []) ~f:(fun acc s ->
              match acc with
              | None -> None
              | Some acc ->
                (match product (prefix ^ q ^ s) qs rest with
                | None -> None
                | Some xs -> Some (List.rev_append xs acc))
          )
          |> Base.Option.map ~f:List.rev
        | _ -> None
      in
      product "" quasis string_lists)
  | _ -> None

let extract_strings = extract_strings_aux ~expand_generics:false

(* If [s] starts at byte [i] with a JS WhiteSpace or LineTerminator codepoint
   (per `String.prototype.trim` semantics), return the codepoint's UTF-8 byte
   length; otherwise 0. Recognizes ASCII whitespace plus the multi-byte cases
   that show up in practice: NBSP (U+00A0), Ogham space (U+1680),
   U+2000..U+200A, line/paragraph separator (U+2028/U+2029),
   narrow/medium no-break space (U+202F/U+205F), ideographic space (U+3000),
   and ZWNBSP/BOM (U+FEFF). *)
let js_whitespace_byte_len s i =
  let n = String.length s in
  if i >= n then
    0
  else
    match s.[i] with
    | ' '
    | '\t'
    | '\n'
    | '\r'
    | '\x0b'
    | '\x0c' ->
      1
    | '\xc2' when i + 1 < n && s.[i + 1] = '\xa0' -> 2
    | '\xe1' when i + 2 < n && s.[i + 1] = '\x9a' && s.[i + 2] = '\x80' -> 3
    | '\xe2' when i + 2 < n && s.[i + 1] = '\x80' ->
      let c2 = Char.code s.[i + 2] in
      if (c2 >= 0x80 && c2 <= 0x8a) || c2 = 0xa8 || c2 = 0xa9 || c2 = 0xaf then
        3
      else
        0
    | '\xe2' when i + 2 < n && s.[i + 1] = '\x81' && s.[i + 2] = '\x9f' -> 3
    | '\xe3' when i + 2 < n && s.[i + 1] = '\x80' && s.[i + 2] = '\x80' -> 3
    | '\xef' when i + 2 < n && s.[i + 1] = '\xbb' && s.[i + 2] = '\xbf' -> 3
    | _ -> 0

(* Lexical validator for the body of `${number}`. Mirrors TS's
   `value !== "" && isFinite(+value)` acceptance rule so a string like "42" is
   a valid match for `${number}`. Concretely:
   - empty string is rejected
   - leading and trailing whitespace are stripped (a whitespace-only string is
     accepted, since `Number(" ")` is 0)
   - signed decimal forms (with optional fraction and exponent) are accepted
   - unsigned hex/octal/binary radix literals are accepted; signed radix is not
     (`Number("-0xff")` is NaN)
   - underscores, NaN, and Infinity are rejected (NaN/Infinity aren't finite;
     underscores aren't valid in `Number()`-coerced strings)
   Strings the strict numeric grammar would reject but `Number()` accepts
   (e.g. " 42" coercing to 42) are handled by trimming JS whitespace at both
   ends via [js_whitespace_byte_len], which covers common multi-byte
   codepoints like NBSP. *)
let is_number_text s =
  let trim s =
    let n = String.length s in
    let lo = ref 0 in
    let stop = ref (n = 0) in
    while not !stop do
      let w = js_whitespace_byte_len s !lo in
      if w = 0 then
        stop := true
      else begin
        lo := !lo + w;
        if !lo >= n then stop := true
      end
    done;
    (* Forward-scan from [lo] tracking the byte offset just past the last
       non-whitespace codepoint. We advance by UTF-8 codepoint widths (read
       from the leading byte) so a non-whitespace multi-byte codepoint
       followed by trailing whitespace ends at the right boundary. *)
    let i = ref !lo in
    let hi = ref !lo in
    while !i < n do
      let w = js_whitespace_byte_len s !i in
      if w > 0 then
        i := !i + w
      else
        let c = Char.code s.[!i] in
        let len =
          if c < 0x80 then
            1
          else if c < 0xc0 then
            1
          else if c < 0xe0 then
            2
          else if c < 0xf0 then
            3
          else
            4
        in
        let len = min len (n - !i) in
        i := !i + len;
        hi := !i
    done;
    String.sub s !lo (!hi - !lo)
  in
  if String.length s = 0 then
    false
  else
    let s = trim s in
    let n = String.length s in
    if n = 0 then
      (* Whitespace-only: `Number(" ")` is 0 (finite). *)
      true
    else
      let is_digit c = c >= '0' && c <= '9' in
      let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') in
      let is_oct c = c >= '0' && c <= '7' in
      let is_bin c = c = '0' || c = '1' in
      let read_digits start =
        let rec loop j =
          if j < n && is_digit s.[j] then
            loop (j + 1)
          else
            j
        in
        loop start
      in
      let all_from start ok =
        start < n
        &&
        let rec loop j = j = n || (ok s.[j] && loop (j + 1)) in
        loop start
      in
      let validate_decimal start =
        let int_end = read_digits start in
        let has_int = int_end > start in
        let (after_frac, has_frac) =
          if int_end < n && s.[int_end] = '.' then
            let f = read_digits (int_end + 1) in
            (f, f > int_end + 1)
          else
            (int_end, false)
        in
        if not (has_int || has_frac) then
          false
        else if after_frac < n && (s.[after_frac] = 'e' || s.[after_frac] = 'E') then
          let after_e = after_frac + 1 in
          let after_sign =
            if after_e < n && (s.[after_e] = '+' || s.[after_e] = '-') then
              after_e + 1
            else
              after_e
          in
          let exp_end = read_digits after_sign in
          exp_end > after_sign && exp_end = n
        else
          after_frac = n
      in
      let signed = s.[0] = '+' || s.[0] = '-' in
      let i =
        if signed then
          1
        else
          0
      in
      if i >= n then
        false
      else if (not signed) && i + 1 < n && s.[i] = '0' then
        (* Radix forms only valid unsigned: `Number("-0xff")` is NaN. *)
        match s.[i + 1] with
        | 'x'
        | 'X' ->
          all_from (i + 2) is_hex
        | 'o'
        | 'O' ->
          all_from (i + 2) is_oct
        | 'b'
        | 'B' ->
          all_from (i + 2) is_bin
        | _ -> validate_decimal i
      else
        validate_decimal i

(* Lexical validator for `${bigint}`: optional `-` sign (no `+`), then either
   `0` alone, a decimal with no leading zero, or a hex/octal/binary literal.
   No fraction, no exponent. *)
let is_bigint_text s =
  let n = String.length s in
  let is_digit c = c >= '0' && c <= '9' in
  let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') in
  let is_oct c = c >= '0' && c <= '7' in
  let is_bin c = c = '0' || c = '1' in
  let all_from start ok =
    start < n
    &&
    let rec loop j = j = n || (ok s.[j] && loop (j + 1)) in
    loop start
  in
  if n = 0 then
    false
  else
    let i =
      if s.[0] = '-' then
        1
      else
        0
    in
    if i >= n then
      false
    else if s.[i] = '0' then
      if i + 1 = n then
        true
      else if i + 1 < n then
        match s.[i + 1] with
        | 'x'
        | 'X' ->
          all_from (i + 2) is_hex
        | 'o'
        | 'O' ->
          all_from (i + 2) is_oct
        | 'b'
        | 'B' ->
          all_from (i + 2) is_bin
        | _ -> false
      else
        false
    else if s.[i] >= '1' && s.[i] <= '9' then
      let rec loop j = j = n || (is_digit s.[j] && loop (j + 1)) in
      loop (i + 1)
    else
      false

let is_bool_text = function
  | "true"
  | "false" ->
    true
  | _ -> false

(* When a concrete string is matched against a TemplateLiteralT containing an
   abstract base type (string, number, bigint, boolean), the matched substring
   is technically not a subtype of that base. We accept it anyway if it
   parses as a valid lexical form for the base, because TS does — that's the
   only way `'42px' as `${number}px`` type-checks. Returns Some validator for
   those base types; None otherwise. Callers using this to short-circuit
   subtyping must skip flowing the matched substring to t, since validation
   has already happened lexically. *)
let rec lexical_validator_of = function
  | DefT (_, StrGeneralT _) -> Some (fun (_ : string) -> true)
  | DefT (_, NumGeneralT _) -> Some is_number_text
  | DefT (_, BigIntGeneralT _) -> Some is_bigint_text
  | DefT (_, BoolGeneralT) -> Some is_bool_text
  (* `Uppercase<X>` etc.: the substring is accepted iff it's in the canonical
     casing form for `kind` AND it satisfies the lexical validator for `arg`.
     This is what makes `'ABC' as Uppercase<\`a${string}\`>` reject 'aBc' but
     accept 'ABC' once the prefix is matched: the inner `${Uppercase<string>}`
     places `'BC'` against `string`-with-uppercase-canonical-form, and 'BC'
     uppercased is itself.

     For `Capitalize` / `Uncapitalize`, an EMPTY substring is rejected here
     even though the empty string is trivially canonical: in placeholder
     context, an empty placeholder means the leading character of the produced
     string comes from a neighboring quasi or interpolation, not from this
     placeholder — so the cap/uncap constraint must not "discharge" on an
     empty match. Callers that want to admit the empty-placeholder case (e.g.
     `Capitalize<\`${string}abc\`>` accepting `'Abc'`) handle it by emitting
     a separate union arm in `String_case_transform.resolve`. *)
  | StringMappingT { kind; arg; _ } ->
    (match lexical_validator_of arg with
    | Some inner ->
      Some (fun s -> String_case_transform.is_canonical_for_placeholder kind s && inner s)
    | None -> None)
  | _ -> None

(* Result of attempting an exact split of a concrete string against a
   TemplateLiteralT's `quasis`/`types`. Drives the SingletonStrT<:TemplateLiteralT
   subtyping rule:
   - Match pairs: the split succeeded; each pair is a (substring, placeholder type)
     that the caller may need to flow.
   - Mismatch: no split works — the string is not a member of the template type.
   - Invariant_violation: quasis/types arities violate the invariant (quasis = types + 1). *)
type match_result =
  | Match of (string * Type.t) list
  | Mismatch
  | Invariant_violation

(* Plan a successful match as a list of (substring, type) pairs to flow,
   without emitting any errors. Returning a result lets the matcher
   backtrack: try one candidate, and if the remaining match fails, try
   the next candidate instead of committing to the first that chops.
   When a type's possible values are concrete, validate the assigned
   substring is in the set during planning so semantic mismatches also
   trigger backtracking (otherwise plan would commit to the first
   structurally-valid split and fail at flow time). *)
let match_string_against_template s quasis types =
  let pair part t =
    match extract_strings t with
    | Some strs ->
      if List.mem part strs then
        Some (part, t)
      else
        None
    | None ->
      (match lexical_validator_of t with
      | Some validate ->
        if validate part then
          Some (part, t)
        else
          None
      | None -> Some (part, t))
  in
  let cons_pair part t pairs =
    match pair part t with
    | None -> None
    | Some p -> Some (p :: pairs)
  in
  let rec plan s quasis types =
    match (quasis, types) with
    | ([q], []) ->
      if s = q then
        Some []
      else
        None
    | (q :: qs, t :: ts) ->
      (match Base.String.chop_prefix s ~prefix:q with
      | None -> None
      | Some rest ->
        (match qs with
        | [last_q] -> match_anchored_to_end rest t last_q
        | next_q :: _ when next_q <> "" -> match_with_delimiter rest t qs ts next_q
        | _ ->
          (* Empty quasi between adjacent types. *)
          (match extract_strings t with
          | Some strs -> match_concrete_prefix rest t qs ts strs
          | None ->
            let next_concrete =
              match ts with
              | next_t :: _ -> extract_strings next_t
              | [] -> None
            in
            (match next_concrete with
            | Some next_strs -> match_lookahead rest t qs ts next_strs
            | None -> match_single_char_step rest t qs ts))))
    | _ -> raise Exit
  (* Last placeholder before the terminal quasi: chop the trailing quasi off
     and pair the middle. *)
  and match_anchored_to_end rest t last_q =
    match Base.String.chop_suffix rest ~suffix:last_q with
    | None -> None
    | Some middle ->
      (match pair middle t with
      | None -> None
      | Some p -> Some [p])
  (* Non-empty quasi between this placeholder and the next: scan the
     remaining string for it; the prefix before each occurrence is a
     candidate substring for this placeholder. Backtrack on subsequent
     failures. *)
  and match_with_delimiter rest t qs ts next_q =
    let next_q_len = String.length next_q in
    let rest_len = String.length rest in
    let rec find_delim i =
      if i + next_q_len > rest_len then
        None
      else if String.sub rest i next_q_len = next_q then
        let part = String.sub rest 0 i in
        let remainder = String.sub rest i (rest_len - i) in
        match plan remainder qs ts with
        | Some pairs ->
          (match cons_pair part t pairs with
          | Some _ as r -> r
          | None -> find_delim (i + 1))
        | None -> find_delim (i + 1)
      else
        find_delim (i + 1)
    in
    find_delim 0
  (* Adjacent placeholders (empty quasi between them) and this placeholder's
     type is a known concrete-string set. Try each candidate as a prefix and
     backtrack if the rest fails. Cases like `${'a' | 'ab'}${T}` against
     'abc' with T='c' must try 'ab' after 'a' fails. *)
  and match_concrete_prefix rest t qs ts strs =
    List.find_map
      (fun candidate ->
        match Base.String.chop_prefix rest ~prefix:candidate with
        | None -> None
        | Some remainder ->
          (match plan remainder qs ts with
          | Some pairs -> cons_pair candidate t pairs
          | None -> None))
      strs
  (* Adjacent placeholders, this placeholder is non-concrete but the next is.
     Scan forward for any occurrence of a next-placeholder candidate; the
     substring before it is assigned to this placeholder. *)
  and match_lookahead rest t qs ts next_strs =
    (* Drop empty candidates: matching at i=0 makes no progress, and any
       non-empty candidate will be tried at its real offset anyway. *)
    let next_strs = List.filter (fun s -> s <> "") next_strs in
    let rest_len = String.length rest in
    let rec find_ahead i =
      if i > rest_len then
        None
      else
        let found =
          List.exists
            (fun candidate ->
              let clen = String.length candidate in
              i + clen <= rest_len && String.sub rest i clen = candidate)
            next_strs
        in
        if found then
          let part = String.sub rest 0 i in
          let remainder = String.sub rest i (rest_len - i) in
          match plan remainder qs ts with
          | Some pairs ->
            (match cons_pair part t pairs with
            | Some _ as r -> r
            | None -> find_ahead (i + 1))
          | None -> find_ahead (i + 1)
        else
          find_ahead (i + 1)
    in
    find_ahead 0
  (* Adjacent placeholders with no concrete anchor on either side: assign
     one character at a time, or the empty string when [rest] is empty
     (later placeholders may still match emptily). *)
  and match_single_char_step rest t qs ts =
    let (char_str, remainder) =
      if String.length rest > 0 then
        (String.sub rest 0 1, String.sub rest 1 (String.length rest - 1))
      else
        ("", "")
    in
    match plan remainder qs ts with
    | Some pairs -> cons_pair char_str t pairs
    | None -> None
  in
  match plan s quasis types with
  | exception Exit -> Invariant_violation
  | None -> Mismatch
  | Some pairs -> Match pairs

(* Fold any placeholder whose type is a single concrete string (or empty union
   thereof) into the adjacent quasis. This normalizes structures like
   `${'px'}` to a quasi `"px"` so that pairwise quasi comparison can succeed
   against an upper bound that has the same characters as literal text rather
   than as a placeholder. Returns (quasis', types') with the same total
   length invariant (quasis = types + 1). *)
let fold_concrete_placeholders quasis types =
  let rec aux acc_q acc_t pending_q quasis types =
    match (quasis, types) with
    | ([q], []) -> (List.rev ((pending_q ^ q) :: acc_q), List.rev acc_t)
    | (q :: qs, t :: ts) ->
      (match extract_strings t with
      | Some [s] -> aux acc_q acc_t (pending_q ^ q ^ s) qs ts
      | _ -> aux ((pending_q ^ q) :: acc_q) (t :: acc_t) "" qs ts)
    | _ -> (List.rev acc_q @ quasis, List.rev acc_t @ types)
  in
  aux [] [] "" quasis types

(* True if `t`, when placed in a template literal placeholder, always coerces
   to a string at runtime: string, number, bigint, boolean, null, undefined,
   and their literal forms are all stringified. Unions are coercible iff
   every member is. Generics are coercible iff their bound is. This is used
   when subtyping `${T}` <: `${string}` — the RHS placeholder accepts any of
   these without needing strict T <: string. *)
let rec placeholder_coerces_to_string = function
  | DefT (_, StrGeneralT _)
  | DefT (_, SingletonStrT _)
  | DefT (_, NumGeneralT _)
  | DefT (_, SingletonNumT _)
  | DefT (_, NumericStrKeyT _)
  | DefT (_, BigIntGeneralT _)
  | DefT (_, SingletonBigIntT _)
  | DefT (_, BoolGeneralT)
  | DefT (_, SingletonBoolT _)
  | DefT (_, NullT)
  | DefT (_, VoidT)
  | TemplateLiteralT _ ->
    true
  | UnionT (_, rep) -> List.for_all placeholder_coerces_to_string (UnionRep.members rep)
  | GenericT { bound; _ } -> placeholder_coerces_to_string bound
  | _ -> false

(* Compute the Cartesian product of string lists, interleaved with quasis.
   quasis = [q0; q1; ...; qn], string_lists = [[s0a; s0b; ...]; [s1a; ...]; ...]
   Result: all combinations of q0 ++ s0x ++ q1 ++ s1y ++ ... ++ qn.
   Returns None if the quasi/type lengths violate the invariant. *)
let cartesian_product quasis string_lists =
  let rec aux prefix quasis string_lists =
    match (quasis, string_lists) with
    | ([q], []) -> Some [prefix ^ q]
    | (q :: qs, ss :: rest) ->
      let combine acc s =
        match acc with
        | None -> None
        | Some acc ->
          (match aux (prefix ^ q ^ s) qs rest with
          | None -> None
          | Some xs -> Some (List.rev_append xs acc))
      in
      (match List.fold_left combine (Some []) ss with
      | None -> None
      | Some xs -> Some (List.rev xs))
    | _ -> None
  in
  aux "" quasis string_lists

let max_cross_product_size = 10_000

(* Try to eagerly resolve all interpolated types to string lists.
   Returns None if any type is not a literal. *)
let try_resolve_eagerly_aux ~expand_generics types =
  Base.List.map types ~f:(extract_strings_aux ~expand_generics) |> Base.Option.all

let try_resolve_eagerly = try_resolve_eagerly_aux ~expand_generics:false

(* Saturating product against [max_cross_product_size]. OCaml int multiplication
   wraps silently in 63-bit two's complement, so a naive fold over, say, 19
   unions of 10 strings each (true product ~10^19) would overflow past max_int
   and the > limit guard could be silently bypassed. Saturate once the cap is
   exceeded so the guard's comparison stays meaningful. *)
let cross_product_size string_lists =
  let cap = max_cross_product_size + 1 in
  List.fold_left
    (fun acc strs ->
      if acc >= cap then
        acc
      else
        let n = List.length strs in
        if n = 0 then
          0
        else if acc > cap / n then
          cap
        else
          acc * n)
    1
    string_lists

(* Post-process a per-type string-lists resolution into the final list of
   producible strings. Returns [None] when the cross product exceeds the
   complexity cap, when any union member was empty (which would otherwise
   silently accept any RHS), or when the resolver itself returned [None]. *)
let finalize_to_strings quasis = function
  | None -> None
  | Some string_lists ->
    if cross_product_size string_lists > max_cross_product_size then
      None
    else (
      match cartesian_product quasis string_lists with
      | Some (_ :: _ as strings) -> Some strings
      | _ -> None
    )

(* Try to resolve a template literal to concrete strings.
   Returns Some strings if all types are concrete, the cross product is within limits,
   and at least one combination exists. Returns None otherwise (including when any
   union member is empty, which would silently accept any RHS).
   When `expand_generics` is true, generics are treated as their bound — useful at
   subtyping time to enumerate the strings a template literal could produce. *)
let try_resolve_to_strings ?(expand_generics = false) quasis types =
  try_resolve_eagerly_aux ~expand_generics types |> finalize_to_strings quasis

(* Subtyping-time variant: concretize each type via Flow_js before extracting.
   The syntactic `extract_strings_aux` only walks UnionT and GenericT, so it
   misses indirected types like `EvalT` (indexed access, mapped/conditional),
   `KeysT`, `OpenT`, `AnnotT`, and `TypeAppT`. At subtyping time we have
   Flow_js access and can resolve those to concrete forms first.
   `possible_concrete_types_for_inspection` is passed as a parameter rather
   than imported directly so this module stays free of Flow_js dependencies
   (it is also called from `type_sig_merge` which deliberately blocks Flow_js). *)
let extract_strings_concretized possible_concrete_types_for_inspection cx t =
  let ts = possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t in
  Base.List.map ts ~f:(extract_strings_aux ~expand_generics:true)
  |> Base.Option.all
  |> Base.Option.map ~f:List.concat

let try_resolve_concretized possible_concrete_types_for_inspection cx types =
  Base.List.map types ~f:(extract_strings_concretized possible_concrete_types_for_inspection cx)
  |> Base.Option.all

(* Concretizing analogue of `try_resolve_to_strings`. *)
let try_resolve_to_strings_concretized possible_concrete_types_for_inspection cx quasis types =
  try_resolve_concretized possible_concrete_types_for_inspection cx types
  |> finalize_to_strings quasis

(* Template literal placeholders must be assignable to
   `string | number | bigint | boolean | null | undefined`. This is a
   *conservative* structural check: it returns true only when we can prove
   the type cannot be a valid placeholder. Things we can't analyze
   (`OpenT`, `AnnotT`, `EvalT`, `KeysT`, custom builtins, etc.) get the
   benefit of the doubt and are allowed through. The full subtyping check
   is too expensive to run here, and not all paths into `resolve` have
   access to Flow_js (type_sig_merge deliberately blocks it). *)
let definitely_invalid_placeholder = function
  | DefT
      ( _,
        ( ObjT _ | ClassT _ | InstanceT _ | FunT _ | SymbolT | UniqueSymbolT _ | ArrT _ | TypeT _
        | EnumValueT _ | EnumObjectT _ | RendersT _ | MixedT _ )
      ) ->
    true
  | _ -> false

(* For a union placeholder, every member must be a valid placeholder type.
   For a generic, recurse through the bound so that explicit bounds like
   `<T: {a: 1}>` are flagged. The one exemption is a `mixed` bound, which
   is how both unconstrained `<T>` and `infer P` are represented in Flow:
   TS implicitly constrains `infer P` in template literal patterns to
   `string`, and the two forms are indistinguishable in our representation,
   so we false-negative on unconstrained `<T>` rather than false-positive on
   `infer P`. *)
let rec definitely_invalid_placeholder_deep t =
  match t with
  | UnionT (_, rep) -> List.exists definitely_invalid_placeholder_deep (UnionRep.members rep)
  | GenericT { bound = DefT (_, MixedT _); _ } -> false
  | GenericT { bound; _ } -> definitely_invalid_placeholder_deep bound
  | _ -> definitely_invalid_placeholder t

(* Build a [SingletonStrT] (singleton), a [UnionT] of [SingletonStrT]s
   (multiple strings), or [None] (empty list — callers handle that case
   context-specifically). [mk_str] constructs the per-element [SingletonStrT]
   so callers can attach their preferred per-element reason. *)
let mk_singleton_or_union ~union_reason ~union_aloc ~mk_str = function
  | [] -> None
  | [s] -> Some (mk_str s)
  | s0 :: s1 :: rest ->
    let t0 = mk_str s0 in
    let t1 = mk_str s1 in
    let ts = List.map mk_str rest in
    Some (UnionT (union_reason, UnionRep.make ~source_aloc:union_aloc t0 t1 ts))

(* Enforce that template literal placeholders are assignable to
   `string | number | bigint | boolean | null | undefined`.

   Two-phase check:
   1. Eagerly: emit immediately for placeholders provably invalid from the
      shape alone ([definitely_invalid_placeholder_deep]).
   2. Deferred: for placeholders we can't prove invalid syntactically, ask
      Flow_js after main inference to concretize types hidden behind
      [EvalT]/[OpenT]/[AnnotT] (indexed access, mapped/conditional types,
      property access) and re-run the predicate on each branch.

   [possible_concrete_types_for_inspection = None] disables the deferred
   phase. That's the [type_sig_merge] path: signature merging deliberately
   blocks Flow_js, and the types it sees aren't yet flowed through subtyping
   so deeper indirection wouldn't be resolvable anyway. *)
let validate_placeholders ?possible_concrete_types_for_inspection cx loc types =
  let emit_error () =
    Flow_js_utils.add_output
      cx
      (Error_message.EInvalidTemplateLiteralType
         { loc; kind = Error_message.InvalidPlaceholderType }
      )
  in
  List.iter
    (fun t ->
      if definitely_invalid_placeholder_deep t then
        emit_error ()
      else
        Base.Option.iter possible_concrete_types_for_inspection ~f:(fun concretize ->
            Context.add_post_inference_validation_callback cx (fun () ->
                (* Concretization can emit its own errors as a side effect (e.g.
                   "indexed access is not an object" when evaluating `T[K]`
                   under an unresolved generic `T`). Those errors are unrelated
                   to the placeholder-validity question we're asking. *)
                let concrete =
                  Context.with_suppressed_errors cx (fun () ->
                      concretize cx (TypeUtil.reason_of_t t) t
                  )
                in
                if List.exists definitely_invalid_placeholder_deep concrete then emit_error ()
            )
        ))
    types

(* Build a Flow type from quasis and interpolated types.
   If all types resolve to literals, returns SingletonStrT or UnionT of SingletonStrTs.
   If the cross product exceeds the size limit, reports an error and falls back to string.
   Otherwise returns TemplateLiteralT.

   Placeholder validity is enforced via [validate_placeholders] before
   construction. *)
let resolve ?possible_concrete_types_for_inspection ~quasis ~types loc cx =
  let unresolved () =
    let reason = mk_annot_reason RTemplateLiteralType loc in
    TemplateLiteralT { reason; quasis; types }
  in
  validate_placeholders ?possible_concrete_types_for_inspection cx loc types;
  if types = [] then
    (* No interpolations: just concatenate quasis *)
    let s = String.concat "" quasis in
    let reason = mk_annot_reason (RStringLit (OrdinaryName s)) loc in
    DefT (reason, SingletonStrT { from_annot = true; value = OrdinaryName s })
  else
    match try_resolve_eagerly types with
    | Some string_lists ->
      if cross_product_size string_lists > max_cross_product_size then begin
        Flow_js_utils.add_output
          cx
          (Error_message.EInvalidTemplateLiteralType { loc; kind = Error_message.TooComplex });
        let reason = mk_annot_reason RTemplateLiteralType loc in
        DefT (reason, StrGeneralT AnyLiteral)
      end else (
        match cartesian_product quasis string_lists with
        | None ->
          Flow_js_utils.add_output
            cx
            (Error_message.EInternal
               ( loc,
                 Error_message.UnexpectedAnnotationInference "TemplateLiteralT quasis/types arity"
               )
            );
          unresolved ()
        | Some strings ->
          let mk_str s =
            let r = mk_annot_reason (RStringLit (OrdinaryName s)) loc in
            DefT (r, SingletonStrT { from_annot = true; value = OrdinaryName s })
          in
          let union_reason = mk_annot_reason RTemplateLiteralType loc in
          let union_aloc = Context.make_aloc_id cx loc in
          (match mk_singleton_or_union ~union_reason ~union_aloc ~mk_str strings with
          | Some t -> t
          | None ->
            (* Empty cross product means a union member was empty; fall back to
               TemplateLiteralT so subtyping doesn't silently accept anything. *)
            unresolved ())
      )
    | None -> unresolved ()

(* ========================================================================= *)
(* Subtyping helpers (callers: subtyping_kit)                                *)
(* ========================================================================= *)

(* Replace each placeholder type with a singleton/union of its stringified
   producible values, when concretization reveals it is fully literal.
   This lets `${T[0]}` (an EvalT indexed access) resolve to `${'1'}` after T
   is substituted to a concrete tuple, so the matcher compares stringified
   forms instead of trying to flow a string into the underlying number/bigint.
   Generic placeholders are NOT expanded — keeping them as-is preserves
   inference for APIs like `<T extends 'a'|'b'>(x: \`get${T}\`): T`. *)
let concretize_placeholders ?possible_concrete_types_for_inspection cx ~upper types =
  match possible_concrete_types_for_inspection with
  | None -> types
  | Some concretize ->
    let union_reason = TypeUtil.reason_of_t upper in
    let union_aloc = Context.make_aloc_id cx (loc_of_reason union_reason) in
    let mk_str s =
      let r = replace_desc_reason (RStringLit (OrdinaryName s)) union_reason in
      DefT (r, SingletonStrT { from_annot = true; value = OrdinaryName s })
    in
    Base.List.map types ~f:(fun t ->
        match extract_strings t with
        | Some _ -> t
        | None ->
          (match extract_strings_concretized concretize cx t with
          | Some ss ->
            (match mk_singleton_or_union ~union_reason ~union_aloc ~mk_str ss with
            | Some t' -> t'
            | None -> t)
          | None -> t)
    )

(* SingletonStrT s <: TemplateLiteralT: decompose the string by quasis and
   flow each extracted segment to the corresponding interpolated type.
   We fold concrete-string placeholders into adjacent quasis first so that
   patterns like `${T}${''}${U}` (which arise when a generic delimiter is
   instantiated to '') normalize to `${T}${U}` — otherwise the empty
   placeholder breaks pairwise decomposition and the match falls through. *)
let subtype_str_lit_into_template
    ?possible_concrete_types_for_inspection ~rec_flow_t cx trace use_op ~lower ~upper s quasis types
    =
  let types = concretize_placeholders ?possible_concrete_types_for_inspection cx ~upper types in
  let (quasis, types) = fold_concrete_placeholders quasis types in
  Flow_js_utils.update_lit_type_from_annot cx lower;
  match match_string_against_template s quasis types with
  | Invariant_violation ->
    Flow_js_utils.add_output
      cx
      (Error_message.EInternal
         ( TypeUtil.loc_of_t upper,
           Error_message.UnexpectedAnnotationInference "TemplateLiteralT quasis/types arity"
         )
      )
  | Mismatch ->
    Flow_js_utils.add_output
      cx
      (Error_message.EIncompatibleDefs
         {
           use_op;
           reason_lower = TypeUtil.reason_of_t lower;
           reason_upper = TypeUtil.reason_of_t upper;
           branches = [];
         }
      )
  | Match pairs ->
    List.iter
      (fun (part, t) ->
        (* Three planning outcomes: lexical validator matched, concrete-string
           set matched, or neither (generic-like). Only the third needs a
           flow — the other two were already validated during planning, and
           flowing a string literal into a non-string type (NullT/VoidT/etc.)
           would emit a spurious error. *)
        match lexical_validator_of t with
        | Some _ -> ()
        | None ->
          (match extract_strings t with
          | Some _ -> ()
          | None ->
            let r =
              replace_desc_reason (RStringLit (OrdinaryName part)) (TypeUtil.reason_of_t lower)
            in
            let str_t = DefT (r, SingletonStrT { from_annot = true; value = OrdinaryName part }) in
            rec_flow_t cx trace ~use_op (str_t, t)))
      pairs

(* TemplateLiteralT <: UnionT: enumerate the strings the LHS could produce
   (expanding generics) and flow each one into the RHS union independently. *)
let subtype_template_into_union ~rec_flow_t cx trace use_op ~lower ~upper quasis types =
  let strings =
    try_resolve_to_strings ~expand_generics:true quasis types |> Base.Option.value ~default:[]
  in
  List.iter
    (fun s ->
      let r = replace_desc_reason (RStringLit (OrdinaryName s)) (TypeUtil.reason_of_t lower) in
      let str_t = DefT (r, SingletonStrT { from_annot = true; value = OrdinaryName s }) in
      rec_flow_t cx trace ~use_op (str_t, upper))
    strings

(* Whether the wide-string rule accepts the LHS template against a RHS of
   shape `prefix${string}suffix`: every LHS placeholder coerces to a string,
   and the LHS's outer quasis (start/end) lexically match the RHS's
   prefix/suffix. Uses raw LHS quasis/types (no folding) to preserve the
   prior inline subtyping-kit guard's semantics. *)
let is_wide_string_template_match lq_raw lt_raw rq rt =
  match (rq, rt) with
  | ([rp; rs], [DefT (_, StrGeneralT _)]) ->
    List.for_all placeholder_coerces_to_string lt_raw
    && (match lq_raw with
       | first :: _ -> String.starts_with ~prefix:rp first
       | [] -> false)
    &&
    let last_q = List.nth lq_raw (List.length lq_raw - 1) in
    String.ends_with ~suffix:rs last_q
  | _ -> false

(* Result of [try_subtype_template_to_template]. [Not_applicable] means no
   structural rule fired — the caller should fall through to the cross-product
   enumeration path. *)
type tl_to_tl_result =
  | Handled
  | Not_applicable

(* Combined dispatch for `TemplateLiteralT <: TemplateLiteralT`. Folds each
   side's quasis/types once, then tries (in order):
   1. Pairwise: equal folded quasi shape → per-placeholder subtyping, with
      `${string}` on the RHS accepting any LHS placeholder that coerces to
      string.
   2. Prefix extension: `lp${lt}` <: `rp${rt}` when lp starts with rp.
   3. Suffix extension: mirror of the prefix rule.
   4. Wide string: any LHS template whose placeholders all coerce to string
      and whose outer quasis match the RHS `prefix${string}suffix` shape.
      Uses raw LHS quasis/types (unfolded), matching the original semantics. *)
let try_subtype_template_to_template
    ~rec_flow_t cx trace use_op l_reason ~upper lq_raw lt_raw rq_raw rt_raw =
  let (lq, lt) = fold_concrete_placeholders lq_raw lt_raw in
  let (rq, rt) = fold_concrete_placeholders rq_raw rt_raw in
  (* If LHS folds to a single concrete string (no remaining placeholders), it
     is semantically a [SingletonStrT]. Reconstruct it and dispatch to the
     [SingletonStrT <: TemplateLiteralT] path so the RHS pattern can be
     decomposed against the concrete string. This is the path recursive
     types like [ReplaceAll] take when an intermediate iteration's
     constructed-template-literal argument has become fully concrete. *)
  match (lq, lt) with
  | ([s], []) ->
    let r = replace_desc_reason (RStringLit (OrdinaryName s)) l_reason in
    let lower = DefT (r, SingletonStrT { from_annot = true; value = OrdinaryName s }) in
    subtype_str_lit_into_template ~rec_flow_t cx trace use_op ~lower ~upper s rq rt;
    Handled
  | _ ->
    if lq = rq then begin
      List.iter2
        (fun lt rt ->
          match rt with
          | DefT (_, StrGeneralT _) when placeholder_coerces_to_string lt -> ()
          | _ -> rec_flow_t cx trace ~use_op (lt, rt))
        lt
        rt;
      Handled
    end else (
      match (lq, lt, rq, rt) with
      | ([lp; ""], [lt_single], [rp; ""], [rt_single]) when String.starts_with ~prefix:rp lp ->
        let chopped = Base.String.chop_prefix_exn lp ~prefix:rp in
        if chopped = "" then
          rec_flow_t cx trace ~use_op (lt_single, rt_single)
        else begin
          let lower =
            TemplateLiteralT { reason = l_reason; quasis = [chopped; ""]; types = [lt_single] }
          in
          rec_flow_t cx trace ~use_op (lower, rt_single)
        end;
        Handled
      | ([""; ls], [lt_single], [""; rs], [rt_single]) when String.ends_with ~suffix:rs ls ->
        let chopped = Base.String.chop_suffix_exn ls ~suffix:rs in
        if chopped = "" then
          rec_flow_t cx trace ~use_op (lt_single, rt_single)
        else begin
          let lower =
            TemplateLiteralT { reason = l_reason; quasis = [""; chopped]; types = [lt_single] }
          in
          rec_flow_t cx trace ~use_op (lower, rt_single)
        end;
        Handled
      | _ ->
        if is_wide_string_template_match lq_raw lt_raw rq_raw rt_raw then
          Handled
        else
          Not_applicable
    )

(* TemplateLiteralT on the left, against any non-TemplateLiteralT upper.
   Enumerate the strings the template could produce and flow as a singleton
   or union. Fall back to a generic string type (Truthy when at least one
   quasi is non-empty, AnyLiteral otherwise) when the cross product can't be
   resolved. *)
let subtype_template_to_other
    ~rec_flow_t ~possible_concrete_types_for_inspection cx trace use_op ~reason ~upper quasis types
    =
  match
    try_resolve_to_strings_concretized possible_concrete_types_for_inspection cx quasis types
  with
  | Some strings ->
    let mk_str s =
      let r = replace_desc_reason (RStringLit (OrdinaryName s)) reason in
      DefT (r, SingletonStrT { from_annot = true; value = OrdinaryName s })
    in
    let union_aloc = Context.make_aloc_id cx (loc_of_reason reason) in
    let lower =
      match mk_singleton_or_union ~union_reason:reason ~union_aloc ~mk_str strings with
      | Some t -> t
      | None ->
        (* Empty cross product is filtered out by try_resolve_to_strings_concretized. *)
        DefT (reason, StrGeneralT AnyLiteral)
    in
    rec_flow_t cx trace ~use_op (lower, upper)
  | None ->
    let literal_kind =
      if List.exists (fun q -> q <> "") quasis then
        Truthy
      else
        AnyLiteral
    in
    rec_flow_t cx trace ~use_op (DefT (reason, StrGeneralT literal_kind), upper)
