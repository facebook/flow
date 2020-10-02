// @flow

//$FlowFixMe (unused)
declare function f (number) : string;


f("asdf");

//$FlowFixMe (unused)
type T = string;

(3 : T);


type U = number;

//$FlowFixMe[incompatible-cast] (used)
("asdf" : U);


const React = require('react');

type Props = {|
    x : number
|};
declare function MyComponent(props: Props): React.Node;

// $FlowFixMe unused
<MyComponent
    x={true}
/>
