//@flow

// React.Element(Type) was behaving differently from React$Element(Type) due to a mishandled
// type destructor case. This tests that the logic stays correct, as all 8 of these should correctly
// typecheck.
const React = require('react');
class Component extends React.Component<{}> {}

declare var a: ExactReactElement_DEPRECATED<Class<Component>>;
a as ExactReactElement_DEPRECATED<React.ElementType>;

const b = <Component />;
b as ExactReactElement_DEPRECATED<React.ElementType>;

declare var c: ExactReactElement_DEPRECATED<Class<Component>>;
c as ExactReactElement_DEPRECATED<React$ElementType>;

const d = <Component />;
d as ExactReactElement_DEPRECATED<React$ElementType>;
