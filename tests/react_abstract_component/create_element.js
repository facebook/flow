//@flow

const React = require('react');

declare var C: React$AbstractComponent<{+foo?: number, +bar: number | string, +baz: number}, mixed, number>;

const _a = <C foo={3} bar="string" baz={4} />;
const _b = <C bar={3} baz={4} />;
const _c = <C baz={4} />; // Error missing bar
const _d = <C bar={3} />; // Error missing baz
