//@flow

type A = {|[string]: number|};
declare var a: A;
a as $Exact<A>;
