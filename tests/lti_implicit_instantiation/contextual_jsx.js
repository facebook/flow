//@flow
const React = require('react');

function Component<T>(props: {foo?: T}): React.Node { return null; }

let x = <Component />; // Error
let y: React.Node = <Component />; // No error

function Wrapper(props: {children: React.Node}) { return null }

const z = (
  <Wrapper>
    <Component />
  </Wrapper>
);

function ComponentWithBound<T: number>(pprops: {a:T}): React$Node {}
<div><ComponentWithBound a={true} /></div>; // error: bool ~> number
