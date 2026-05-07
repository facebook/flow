// @flow

type Inexact = { data: string; type: string; }
type Exact = { data: string; type: string; }

function foo() {
  return [
    {data: "", type: ""},
    {data: "", type: ""},
  ];
}

foo() as Array<Inexact>;
foo() as Array<Exact>;

module.exports = foo;
