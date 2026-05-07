/**
 * @flow
 */

type O = {p: null} & E;
declare const o: O;

const x = o.p; // no error here
x.nope; // error here

type E = {};
