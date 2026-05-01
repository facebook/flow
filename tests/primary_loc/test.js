//$FlowFixMe (unused)
declare function f (x: number) : string;


f("asdf");

//$FlowFixMe (unused)
type T = string;

3 as T;


type U = number;

//$FlowFixMe[incompatible-type] (used)
"asdf" as U;
