type state = {
  mutable source: Loc.filename option;
}

let state = {
  source = None;
}

let with_source source fn =
  try
    state.source <- source;
    fn ()
  with e ->
    state.source <- None;
    raise e

let curr_source () = state.source
