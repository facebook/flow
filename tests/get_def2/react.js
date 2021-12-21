// @flow

var React = require('react');

type Props = { x: string };
class C extends React.Component<Props> {
}

let msg = "hello";

(<C x={msg}/>);
//^

(<C x={msg}/>);
//  ^

(<C x={msg}/>);
//     ^

// TODO give some result for the JSX intrinsic here
(<div id={msg}/>);
// ^

(<div id={msg}/>);
//         ^
