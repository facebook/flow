//@flow

const React = require('react');

type Props1 = {  a : number  }
type Props2 = {| b : string |}

class Component1 extends React.Component<Props1>{}
class Component2 extends React.Component<Props2>{}

function takesTop(e : React.MixedElement) {
}

takesTop(<Component1 a={1}/>);
takesTop(<Component2 b={"B"}/>);
takesTop(<div/>)

function takesAny(e : React$Element<any>) {
}

takesAny(<Component1 a={1}/>);
takesAny(<Component2 b={"B"}/>);
takesAny(<div/>)
