//@flow
const React = require('react');

declare function HOC<Props extends {...}>(
    x: component(...Props),
): component(...Props);

class A extends React.Component<{...}> {}

module.exports = HOC(A); // Error, missing annotation
