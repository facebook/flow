// @flow

type State = {|
  foo: string,
|};

declare function update(
  bar: ($Shape<State>) => number,
): void;

update((prevState: State) => 0);
