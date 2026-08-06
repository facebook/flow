type T = { ['m'](): X };
type U = { [42](): X };
type V = { [-1](): X };
type W = { [1n](): X };
type Y = { [-1n](): X };
type Z = { [true](): X };
type A = { [null](): X };
