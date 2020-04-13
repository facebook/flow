//@flow

type Props<TValue> = $ReadOnly<{||}>;

type Props2<TValue> = $ReadOnly<{| x?: any, |}>;

declare function foo<TValue>(Props<TValue>): null;

declare function bar<TValue>(props: Props2<TValue>): null;

(foo : typeof bar);
