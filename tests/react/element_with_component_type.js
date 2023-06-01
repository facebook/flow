const React = require('react');

class Foo extends React.Component<{a: number}> {}

(<Foo a={42}/>: React.Element<React.ComponentType<{a: number}>>); // OK
(<Foo a={42}/>: React.Element<React.ComponentType<{b: number}>>); // Error

// React.Element can support 2 type arguments, like React$Element
type El = React.Element<any, any>; // No errors!
