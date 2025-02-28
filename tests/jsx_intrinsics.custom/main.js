var React = require('react');

<div id="asdf" />;
<div id={42} />; // Error: (`id` prop) number ~> string

const dataProps: {[StringPrefix<'data-'>]: string} = {};
const d = <div {...dataProps} />; // OK with data props, but missing id
