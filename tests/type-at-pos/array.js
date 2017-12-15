// @flow

type NumType = Array<{|+nums: number|}>;
type ReadOnlyNumType = $ReadOnlyArray<{|+nums: number|}>;

function foo(num: NumType) {
  num[0];
}

function bar(num: ReadOnlyNumType) {
  num[0];
}
