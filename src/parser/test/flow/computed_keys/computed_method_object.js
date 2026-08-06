type T = { ['m'](): X };
type U = { ['m'](): X, [string]: Y };
type V = { ['m']?(): X };
type W = { ['g']<A>(x: A): A };
