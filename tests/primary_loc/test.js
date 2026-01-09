//$FlowFixMe (unused)
declare function f (x: number) : string;


f("asdf");

//$FlowFixMe (unused)
type T = string;

(3 : T);


type U = number;

//$FlowFixMe[incompatible-type] (used)
("asdf" : U);
