// @flow
module.exports = 0;
export * as ns from "./dependency"; // error
(0: string); // no error b/c we skip check
