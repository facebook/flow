//@flow
const React = require('react');

type ViewNativeComponentType = Class<React.Component<{}>>;

declare const View: ViewNativeComponentType;

module.exports = View;
