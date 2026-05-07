// @flow

type Key = 'A';
declare opaque type Val;

declare const key: Key;
declare const val: Val

module.exports = { [key]: val };
