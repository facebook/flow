const React = require('react');

class Foo extends React.Component<{a: number}> {}

(<Foo a={42}/>: ExactReactElement_DEPRECATED<React.ComponentType<{a: number}>>); // OK
(<Foo a={42}/>: ExactReactElement_DEPRECATED<React.ComponentType<{b: number}>>); // Error

// ExactReactElement_DEPRECATED can support 2 type arguments, like React$Element
type El = ExactReactElement_DEPRECATED<any, any>; // No errors!
