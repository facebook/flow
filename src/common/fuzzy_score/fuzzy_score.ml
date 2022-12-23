(*
 * Portions Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
 * Based on the TypeScript implementation from VSCode:
 * https://github.com/microsoft/vscode/blob/e79a401ba5f6bc8eff07bfffaf4544e96f394837/src/vs/base/common/filters.ts#L574
 * That implementation is...
 *
 *   Copyright (c) Microsoft Corporation. All rights reserved.
 *)

let max_len = 128

let min_safe_integer = -9007199254740991

let max_safe_integer = 9007199254740991

type arrow =
  | Empty
  | Diag
  | Left
  | Left_left

let arrows = Array.make_matrix max_len max_len Empty

let diag = Array.make_matrix max_len max_len 0

let table = Array.make_matrix max_len max_len 0

let length x = Base.Int.min (String.length x) max_len

(** Determines if the chars in [pattern] exist, in order, in [word].

    If [min_word_match_pos] is passed, it is updated (in place) with
    the index of the first occurence of each pattern char in [word].
    The caller must ensure that the array is the same length as
    [pattern]. *)
let fuzzy_match ?min_word_match_pos ~char_equal ?(pattern_start = 0) ~pattern ?(word_start = 0) word
    =
  let pattern_len = String.length pattern in
  let word_len = String.length word in
  let rec walk p w =
    if p = pattern_len then
      (* matched the whole pattern *)
      true
    else if w = word_len then
      (* reached the end of the word without finding all of pattern *)
      false
    else
      let p_c = String.unsafe_get pattern p in
      let w_c = String.unsafe_get word w in
      if char_equal p_c w_c then (
        (match min_word_match_pos with
        | Some arr -> arr.(p) <- w
        | None -> ());
        walk (p + 1) (w + 1)
      ) else
        walk p (w + 1)
  in
  walk pattern_start word_start

let make_max_word_match_pos ~char_equal ~pattern ~pattern_start ~word ~word_start =
  let pattern_len = length pattern in
  let arr = Array.make pattern_len 0 in
  let rec walk p w =
    if p >= pattern_start && w >= word_start then
      let p_c = String.unsafe_get pattern p in
      let w_c = String.unsafe_get word w in
      if char_equal p_c w_c then (
        arr.(p) <- w;
        walk (p - 1) (w - 1)
      ) else
        walk p (w - 1)
  in
  walk (length pattern - 1) (length word - 1);
  arr

let char_equal_lower = Base.Char.Caseless.equal

let is_separator_at_pos value index =
  if index < 0 || index >= String.length value then
    false
  else
    match value.[index] with
    | '_'
    | '-'
    | '.'
    | ' '
    | '/'
    | '\\'
    | '\''
    | '"'
    | ':'
    | '$'
    | '<'
    | '>'
    | '('
    | ')'
    | '['
    | ']'
    | '{'
    | '}' ->
      true
    | _ -> false

let is_whitespace_at_pos value index =
  if index < 0 || index >= String.length value then
    false
  else
    match value.[index] with
    | ' '
    | '\t' ->
      true
    | _ -> false

let is_uppercase_at_pos pos word = Char.equal word.[pos] (Base.Char.uppercase word.[pos])

let do_score
    ~pattern
    ~pattern_pos
    ~pattern_start
    ~word
    ~word_pos
    ~word_len
    ~word_start
    new_match_start
    has_strong_first_match =
  if not (char_equal_lower pattern.[pattern_pos] word.[word_pos]) then
    min_safe_integer
  else
    let (score, is_gap_location) =
      if word_pos = pattern_pos - pattern_start then
        (* common prefix: `foobar <-> foobaz`
                                      ^^^^^ *)
        let score =
          if Char.equal pattern.[pattern_pos] word.[word_pos] then
            7
          else
            5
        in
        (score, false)
      else if
        is_uppercase_at_pos word_pos word
        && (word_pos = 0 || not (is_uppercase_at_pos (word_pos - 1) word))
      then
        (* hitting upper-case: `foo <-> forOthers`
                                        ^^ ^ *)
        let score =
          if Char.equal pattern.[pattern_pos] word.[word_pos] then
            7
          else
            5
        in
        (score, true)
      else if
        is_separator_at_pos word word_pos
        && (word_pos = 0 || not (is_separator_at_pos word (word_pos - 1)))
      then
        (* hitting a separator: `. <-> foo.bar`
                                          ^ *)
        (5, false)
      else if is_separator_at_pos word (word_pos - 1) || is_whitespace_at_pos word (word_pos - 1)
      then
        (* post separator: `foo <-> bar_foo`
                                        ^^^ *)
        (5, true)
      else
        (1, false)
    in

    if score > 1 && pattern_pos = pattern_start then has_strong_first_match := true;

    let is_gap_location =
      if not is_gap_location then
        is_uppercase_at_pos word_pos word
        || is_separator_at_pos word (word_pos - 1)
        || is_whitespace_at_pos word (word_pos - 1)
      else
        is_gap_location
    in

    let score =
      if pattern_pos = pattern_start then
        (* first character in pattern *)
        if word_pos > word_start then
          (* the first pattern character would match a word character that is not at the word start
             so introduce a penalty to account for the gap preceding this match *)
          if is_gap_location then
            score - 3
          else
            score - 5
        else
          score
      else if new_match_start then
        (* this would be the beginning of a new match (i.e. there would be a gap before this location) *)
        if is_gap_location then
          score + 2
        else
          score
      else if
        (* this is part of a contiguous match, so give it a slight bonus, but do so only if it would not be a preferred gap location *)
        is_gap_location
      then
        score
      else
        score + 1
    in

    let score =
      if word_pos + 1 = word_len then
        (* we always penalize gaps, but this gives unfair advantages to a match that would match the last character in the word
           so pretend there is a gap after the last character in the word to normalize things *)
        if is_gap_location then
          score - 3
        else
          score - 5
      else
        score
    in

    score

let fuzzy_score
    ?(boost_full_match = true)
    ?(first_match_can_be_weak = false)
    ?(pattern_start = 0)
    ?(word_start = 0)
    ~pattern
    word =
  let pattern_len = length pattern in
  let word_len = length word in
  if
    pattern_start >= pattern_len
    || word_start >= word_len
    || pattern_len - pattern_start > word_len - word_start
  then
    None
  else
    let min_word_match_pos = Array.make pattern_len 0 in

    (* Run a simple check if the characters of pattern occur
       (in order) at all in word. If that isn't the case we
       stop because no match will be possible

       Also fills in [min_word_match_pos] *)
    let fuzzy_matches =
      fuzzy_match
        ~char_equal:char_equal_lower
        ~min_word_match_pos
        ~pattern_start
        ~word_start
        ~pattern
        word
    in
    if not fuzzy_matches then
      None
    else
      (* Find the max matching word position for each pattern position *)
      let max_word_match_pos =
        make_max_word_match_pos
          ~char_equal:char_equal_lower
          ~pattern
          ~pattern_start
          ~word
          ~word_start
      in

      let has_strong_first_match = ref false in

      let rec fill_rows row column pattern_pos =
        if pattern_pos >= pattern_len then
          (row, column)
        else
          (* Reduce search space to possible matching word positions and to possible access from next row *)
          let min_word_match_pos_ = min_word_match_pos.(pattern_pos) in
          let max_word_match_pos_ = max_word_match_pos.(pattern_pos) in
          let next_max_word_match_pos =
            if pattern_pos + 1 < pattern_len then
              max_word_match_pos.(pattern_pos + 1)
            else
              word_len
          in

          let rec fill_columns_unsafe column word_pos =
            if word_pos >= next_max_word_match_pos then
              column
            else
              let score = min_safe_integer in

              let score =
                if word_pos <= max_word_match_pos_ then
                  do_score
                    ~pattern
                    ~pattern_pos
                    ~pattern_start
                    ~word
                    ~word_pos
                    ~word_len
                    ~word_start
                    (diag.(row - 1).(column - 1) = 0)
                    has_strong_first_match
                else
                  score
              in

              let diag_score = 0 in
              let (can_come_diag, diag_score) =
                if score <> max_safe_integer then
                  (true, score + table.(row - 1).(column - 1))
                else
                  (false, diag_score)
              in

              let can_come_left = word_pos > min_word_match_pos_ in
              let left_score =
                if can_come_left then
                  table.(row).(column - 1)
                  +
                  if diag.(row).(column - 1) > 0 then
                    (* penalty for a gap start *)
                    -5
                  else
                    0
                else
                  0
              in

              let can_come_left_left =
                word_pos > min_word_match_pos_ + 1 && diag.(row).(column - 1) > 0
              in
              let left_left_score =
                if can_come_left_left then
                  table.(row).(column - 2)
                  +
                  if diag.(row).(column - 2) > 0 then
                    -5
                  else
                    0
                else
                  0
              in

              if
                can_come_left_left
                && ((not can_come_left) || left_left_score >= left_score)
                && ((not can_come_diag) || left_left_score >= diag_score)
              then (
                (* always prefer choosing left left to jump over a diagonal because that means a match is earlier in the word *)
                table.(row).(column) <- left_left_score;
                arrows.(row).(column) <- Left_left;
                diag.(row).(column) <- 0
              ) else if can_come_left && ((not can_come_diag) || left_score >= diag_score) then (
                (* always prefer choosing left since that means a match is earlier in the word *)
                table.(row).(column) <- left_score;
                arrows.(row).(column) <- Left;
                diag.(row).(column) <- 0
              ) else if can_come_diag then (
                table.(row).(column) <- diag_score;
                arrows.(row).(column) <- Diag;
                diag.(row).(column) <- diag.(row - 1).(column - 1) + 1
              ) else
                failwith "not possible";
              fill_columns (column + 1) (word_pos + 1)
          and fill_columns column word_pos =
            try fill_columns_unsafe column word_pos with
            | Invalid_argument msg as exn ->
              let exn = Exception.wrap exn in
              let msg =
                Printf.sprintf
                  "%s (pattern: %S, word: %S, row: %d, column: %d)"
                  msg
                  pattern
                  word
                  row
                  column
              in
              Exception.raise_with_backtrace (Invalid_argument msg) exn
          in
          let column = fill_columns (min_word_match_pos_ - word_start + 1) min_word_match_pos_ in

          fill_rows (row + 1) column (pattern_pos + 1)
      in

      let (row, column) = fill_rows 1 1 pattern_start in

      if (not !has_strong_first_match) && not first_match_can_be_weak then
        None
      else
        let row = row - 1 in
        let column = column - 1 in

        let rec find_diag arrows_row diag_column =
          match arrows_row.(diag_column) with
          | Left_left ->
            let diag_column = diag_column - 2 in
            if diag_column >= 1 then
              find_diag arrows_row diag_column
            else
              diag_column
          | Left ->
            let diag_column = diag_column - 1 in
            if diag_column >= 1 then
              find_diag arrows_row diag_column
            else
              diag_column
          | Diag
          | Empty ->
            (* found the diagonal *)
            diag_column
        in

        let rec iter ~backwards_diag_length ~max_match_column row column =
          if row < 1 then
            max_match_column
          else
            (* Find the column where we go diagonally up *)
            let diag_column = find_diag arrows.(row) column in

            (* Overturn the "forwards" decision if keeping the "backwards" diagonal would give a better match *)
            let diag_column =
              if
                backwards_diag_length > 1
                (* only if we would have a contiguous match of 3 characters *)
                && char_equal_lower pattern.[pattern_start + row - 1] word.[word_start + column - 1]
                (* only if we can do a contiguous match diagonally *)
                && (not (is_uppercase_at_pos (diag_column + word_start - 1) word))
                (* only if the forwards chose diagonal is not an uppercase *)
                && backwards_diag_length + 1 > diag.(row).(diag_column)
                (* only if our contiguous match would be longer than the "forwards" contiguous match *)
              then
                column
              else
                diag_column
            in

            let backwards_diag_length =
              if diag_column = column then
                (* this is a contiguous match *)
                backwards_diag_length + 1
              else
                1
            in

            let max_match_column =
              if max_match_column = 0 then
                (* remember the last matched column *)
                diag_column
              else
                max_match_column
            in

            iter ~backwards_diag_length ~max_match_column (row - 1) (diag_column - 1)
        in

        let max_match_column = iter ~backwards_diag_length:0 ~max_match_column:0 row column in

        let result = table.(row).(column) in
        let result =
          if word_len = pattern_len && boost_full_match then
            (* the word matches the pattern with all characters!
               giving the score a total match boost (to come up ahead other words) *)
            result + 2
          else
            result
        in

        (* Add 1 penalty for each skipped character in the word *)
        let skipped_chars_count = max_match_column - pattern_len in
        let result = result - skipped_chars_count in

        Some result
