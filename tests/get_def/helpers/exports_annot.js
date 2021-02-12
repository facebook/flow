// @flow

type X = string

type Y = number

export const x : X = "a string";

export const _y : Y = 123;

export default x;

declare export var y : typeof _y;

declare export var z : boolean;
