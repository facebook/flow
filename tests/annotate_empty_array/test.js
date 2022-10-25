//@flow
import * as React from 'react';

declare var key : string;

let x = []; // no need for annotation
x[0] = 3;

let y = []; // no need for annotation
y.push(4);

let z = []; // should annot
z = [4, 5];

declare var arr : Array<string>;

let first = []; // annot
let second = first;
let third = true ? first : [];
let fourth = third;
fourth[0] = 3;

let already_annotated : number[] = []; // should not change this

let written_twice = []; // should be Array<number | string>
written_twice[0] = 3;
written_twice.push("foo");

let written_thrice = []; // should be Array<number | string>
written_thrice[0] = 3;
written_thrice[1] = "foo";
written_thrice.push("bar", 4);

let z2 = [];

let arr2 = ["A", "B", "C"];
let x2 = []; // no need for annotation

for (let k of arr2) {
  x2[0] = k
}

var x3 = []; // Annotate with Array<number | string>
x3.push(42);
x3.push("a");

function foo() {
  var x = []; // Annotate with Array<number | string>
  x.push(42);
  x.push("a");
}

function bar() {
  var x = []; // Annotate with Array<?string>
  x.push(null);
  x.push("a");
}

function MixedElement() {
  const x = []; // Annotate with Array<React.MixedElement>;
  x.push(<div />);
  x.push(<span />);

  const y = []; // Annotate with Array<React.MixedElement>;
  function Comp(_: {foo: string}): React.Node {};
  y.push(<Comp />);
  y.push(<span />);
}
