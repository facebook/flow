//@flow

type A = {[string]: number};
declare const a: A;
a as $Exact<A>;
