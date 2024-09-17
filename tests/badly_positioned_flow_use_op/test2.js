type T = <A>((A) => mixed) => (A & A);
type fn = (arg: string) => number;
type arg = fn extends ((infer A) => mixed) ? A & A : empty;
const t: arg = 5;
