/* @flow */

type Plop = {|
  prop: string[]
|};

var a: Plop = {
  prop: Array(5).fill(1) // error
};
