const React = require('react');

const dataProps: {[StringPrefix<'data-'>]: string} = {};
const d = <div {...dataProps} />; // OK
