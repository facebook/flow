// Regression test for using ground_eq to summarize solution of implicit instantiation

export type TRefFor<-TElement> =
  | {-current: TElement | null, ...}
  | ((TElement | null) => mixed);

type RefType<TElement> = TRefFor<TElement>;

declare var ref1: { current: {...} | null };
declare var ref2: TRefFor<{...}>;

declare function mergeRefs<TElement>(
  ref1: RefType<TElement>,
  ref2: RefType<TElement>,
): void;

const mergedRefs = mergeRefs(ref1, ref2);
