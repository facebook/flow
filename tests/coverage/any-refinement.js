// @flow

declare var a: any;

if (typeof a === "string") {
  a; // covered
}

if (typeof a === "boolean") {
  a; // covered
}

if (typeof a === "number") {
  a; // covered
}
