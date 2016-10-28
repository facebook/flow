val with_source: Loc.filename option -> (unit -> 'a) -> 'a

val curr_source: unit -> Loc.filename option
