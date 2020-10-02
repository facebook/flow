// @flow

const React = require('React');
const react = require('react');

// React is treated as object of named exports
("" : React.Node);
("" : react.Node);

("" : React.NotAType); // error NotAType missing in React
("" : react.NotAType); // error NotAType missing in react
