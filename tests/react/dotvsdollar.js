//@flow

// React.Element(Type) was behaving differently from React$Element(Type) due to a mishandled
// type destructor case. This tests that the logic stays correct, as all 8 of these should correctly
// typecheck.
const React = require('react');
class Component extends React.Component<{}> {}

declare var a: React.Element<Class<Component>>;
a as React.Element<React.ElementType>;

const b = <Component />;
b as React.Element<React.ElementType>;

declare var c: React.Element<Class<Component>>;
c as React.Element<React$ElementType>;

const d = <Component />;
d as React.Element<React$ElementType>;

declare var e: React.Element<Class<Component>>;
// $FlowFixMe[internal-type] This test is specially testing dot vs internal dollar type
d as React$Element<React$ElementType>;

const f = <Component />;
// $FlowFixMe[internal-type] This test is specially testing dot vs internal dollar type
f as React$Element<React$ElementType>;

declare var g: React.Element<Class<Component>>;
// $FlowFixMe[internal-type] This test is specially testing dot vs internal dollar type
g as React$Element<React.ElementType>;

const h = <Component />;
// $FlowFixMe[internal-type] This test is specially testing dot vs internal dollar type
h as React$Element<React.ElementType>;
