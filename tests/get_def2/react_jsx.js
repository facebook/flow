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

const Foo = {Bar: C, Baz: {Bar: C}};
<Foo.Bar x={msg} />;
// ^
<Foo.Bar x={msg} />;
//    ^

<Foo.Baz.Bar x={msg} />;
// ^
<Foo.Baz.Bar x={msg} />;
//    ^
<Foo.Baz.Bar x={msg} />;
//        ^
