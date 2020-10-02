// @flow

declare export function is_string(x: mixed): boolean %checks(typeof x === "string");
declare export function is_number(x: mixed): boolean %checks(typeof x === "number");
