// @flow

declare function foo<Args: $ReadOnlyArray<mixed>>(
    f: (...Args) => mixed,
  ): (...Args) => void;

declare var x: (string, number) => void;

foo(x)('a', 1); // should annotate with [string, number] to not cause new flow errors
