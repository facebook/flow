// @flow

declare opaque type Val;
declare const obj: { a: Val, b: Val };

module.exports = { ...obj };
