// @flow

const React = require('react');

class A extends React.Component {
  props: { children: string };
}

<A />; // error: property `children` not found
<A></A>; // error: property `children` not found
<A children={0} />; // error: number ~> string
<A children="" />; // ok
<A>{0}</A>; // error: number ~> string
<A> </A>; // ok: jsx whitespace same line rule
<A children={0}> </A>; // ok: jsx children override props
<A children={""}>{null}</A>; // error: null ~> string
<A children={""}>{void 0}</A>; // TODO error: void ~> string
<A children={""}>{}</A>; // ok: empty jsx expr does not override

// jsx whitespace multiline rule
(
  <A children={0}>
  </A>
); // error: number ~> string

// strings are coalesced
(
  <A>
    x
    x
  </A>
); // ok

// jsx expr container delimits strings, even empty ones
(
  <A>
    x
    {}
    x
  </A>
); // error: array ~> string

// empty jsx expr containers collapse with whitespace
(
  <A>
    x
    {}
  </A>
); // ok
(
  <A>
    {}
    x
  </A>
); // ok
(
  <A>
    {}
    {}
  </A>
); // error: property `children` not found

// preserve whitespace between jsx exprs
(
  <A>
    {} {}
  </A>
); // ok

class B extends React.Component {
  props: { children: "a b" };
}

// whitespace is always coalesced to one space within a line and trimmed at the
// end of a line when adjacent to another tag or expression container, but is
// _not_ trimmed when it splits two lines of text
(
  <B>
    a
    b
    {}
  </B>
); // TODO ok: actually "a b" but we don't handle literal text values precisely
