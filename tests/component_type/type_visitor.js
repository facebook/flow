//@flow
const React = require('react');

declare function HOC<Config: {...}, Instance>(
    x: component(ref: React.RefSetter<Instance>, ...Config),
): component(ref: React.RefSetter<Instance>, ...Config);

class A extends React.Component<{...}> {}

module.exports = HOC(A); // Error, missing annotation only for Config
