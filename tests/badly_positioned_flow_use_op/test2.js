type T = <A>((A) => unknown) => A & A;
type fn = (arg: string) => number;
type arg = fn extends (infer A) => unknown ? A & A : empty;
const t: arg = 5;
