external compare : 'a -> 'a -> int = "%compare"

external ascending : 'a -> 'a -> int = "%compare"

let descending x y = compare y x

let ( < ) = ( < )

let ( <= ) = ( <= )

let ( > ) = ( > )

let ( >= ) = ( >= )

let ( = ) = ( = )

let ( <> ) = ( <> )

let equal = ( = )

let min = min

let max = max
