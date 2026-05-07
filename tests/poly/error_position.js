//@flow

type Props<TValue> = Readonly<{}>;

type Props2<TValue> = Readonly<{x?: any}>;

declare function foo<TValue>(props: Props<TValue>): null;

declare function bar<TValue>(props: Props2<TValue>): null;

foo as typeof bar;
