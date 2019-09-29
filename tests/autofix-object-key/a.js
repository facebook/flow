// @flow

type Key = 'A' | 'B';
declare opaque type Val;

declare var key: Key;
declare var val: Val

module.exports = { [key]: val };
