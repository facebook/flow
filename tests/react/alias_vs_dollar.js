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

declare var e: ExactReactElement_DEPRECATED<Class<Component>>;
// $FlowFixMe[internal-type] This test is specially testing dot vs internal dollar type
d as React$Element<React$ElementType>;

const f = <Component />;
// $FlowFixMe[internal-type] This test is specially testing dot vs internal dollar type
f as React$Element<React$ElementType>;

declare var g: ExactReactElement_DEPRECATED<Class<Component>>;
// $FlowFixMe[internal-type] This test is specially testing dot vs internal dollar type
g as React$Element<React.ElementType>;

const h = <Component />;
// $FlowFixMe[internal-type] This test is specially testing dot vs internal dollar type
h as React$Element<React.ElementType>;
