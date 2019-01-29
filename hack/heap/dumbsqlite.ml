type sqlerr = int

type sqlerr_t = { sqlerr : sqlerr }

type classified_error =
  | SqliteOk
  | Row
  | Done
  | SqliteErr of int
  | WrapperErr of int

let classify_sqlerr {sqlerr} =
  match sqlerr with
  | 0 -> SqliteOk
  | 100 -> Row
  | 101 -> Done
  | _ -> (
      if sqlerr > 0 then SqliteErr sqlerr
      else WrapperErr sqlerr
  )

external caml_dumb_sqlite_open : int -> string -> bool -> sqlerr = "caml_dumb_sqlite_open"
let caml_dumb_sqlite_open ~index ~path ~readonly =
  {sqlerr = caml_dumb_sqlite_open index path readonly}

external caml_dumb_sqlite_close : int -> sqlerr = "caml_dumb_sqlite_close"
let caml_dumb_sqlite_close ~index =
  {sqlerr = caml_dumb_sqlite_close index}

external caml_dumb_sqlite_prepare : int -> int -> string -> sqlerr = "caml_dumb_sqlite_prepare"
let caml_dumb_sqlite_prepare ~index ~s_index ~sql =
  {sqlerr = caml_dumb_sqlite_prepare index s_index sql}

external caml_dumb_sqlite_reset : int -> sqlerr = "caml_dumb_sqlite_reset"
let caml_dumb_sqlite_reset ~s_index =
  {sqlerr = caml_dumb_sqlite_reset s_index}

external caml_dumb_sqlite_step : int -> sqlerr = "caml_dumb_sqlite_step"
let caml_dumb_sqlite_step ~s_index =
  {sqlerr = caml_dumb_sqlite_step s_index}

external caml_dumb_sqlite_finalize : int -> sqlerr = "caml_dumb_sqlite_finalize"
let caml_dumb_sqlite_finalize ~s_index =
  {sqlerr = caml_dumb_sqlite_finalize s_index}

external caml_dumb_sqlite_bind_int64 : int -> int -> Int64.t -> sqlerr = "caml_dumb_sqlite_bind_int64"
let caml_dumb_sqlite_bind_int64 ~s_index ~param ~value =
  {sqlerr = caml_dumb_sqlite_bind_int64 s_index param value}

external caml_dumb_sqlite_bind_text : int -> int -> string -> sqlerr = "caml_dumb_sqlite_bind_text"
let caml_dumb_sqlite_bind_text ~s_index ~param ~value =
  {sqlerr = caml_dumb_sqlite_bind_text s_index param value}

external caml_dumb_sqlite_column_int64 : int -> int -> (sqlerr * Int64.t) = "caml_dumb_sqlite_column_int64"
let caml_dumb_sqlite_column_int64 ~s_index ~column =
  let (sqlerr, res) = caml_dumb_sqlite_column_int64 s_index column in
  ({sqlerr = sqlerr}, res)

external caml_dumb_sqlite_column_text : int -> int -> (sqlerr * string) = "caml_dumb_sqlite_column_text"
let caml_dumb_sqlite_column_text ~s_index ~column =
  let (sqlerr, res) = caml_dumb_sqlite_column_text s_index column in
  ({sqlerr = sqlerr}, res)
