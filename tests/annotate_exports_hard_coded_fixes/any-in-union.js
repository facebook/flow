// @flow

const React = require('react');

declare const x1: { f: number , ...} | any;
declare const x2: any | { f: number , ...};
declare const x3: any | { f: number , ...} | { node: React.Node , ...};
declare const x4: { f: number , ...} | any | { node: React.Node , ...};
declare const x5: { f: number , ...} | { node: React.Node , ...} | any;

module.exports = [
  () => x1,
  () => x2,
  () => x3,
  () => x4,
  () => x5,
];
