// @flow

/////////////
// boolean //
/////////////
enum B of boolean {
  A = true,
  B = false,
}

declare var bVoidable: void | B;

if (typeof bVoidable === "undefined") {
  (bVoidable: void); // Valid
  (bVoidable: B); // Error
}

if (typeof bVoidable !== "undefined") {
  (bVoidable: void); // Error
  (bVoidable: B); // Valid
}

if (typeof bVoidable === "boolean") {
  (bVoidable: void); // Error
  (bVoidable: B); // Valid
}

if (typeof bVoidable !== "boolean") {
  (bVoidable: void); // Valid
  (bVoidable: B); // Error
}

declare var bMaybe: ?B;

if (bMaybe == null) {
  (bMaybe: null | void); // Valid
  (bMaybe: B); // Error
}

if (bMaybe != null) {
  (bMaybe: null); // Error
  (bMaybe: void); // Error
  (bMaybe: B); // Valid
}

declare var bBoolVoid: B | boolean | void;

if (typeof bBoolVoid === "boolean") {
  (bBoolVoid: void); // Error
  (bBoolVoid: boolean); // Error
  (bBoolVoid: B); // Error
  (bBoolVoid: B | boolean); // Valid
}

if (typeof bBoolVoid !== "boolean") {
  (bBoolVoid: void); // Valid
  (bBoolVoid: boolean); // Error
  (bBoolVoid: B); // Error
  (bBoolVoid: B | boolean); // Error
}

////////////
// number //
////////////
enum N of number {
  A = 0,
  B = 1,
}

declare var nVoidable: void | N;

if (typeof nVoidable === "undefined") {
  (nVoidable: void); // Valid
  (nVoidable: N); // Error
}

if (typeof nVoidable !== "undefined") {
  (nVoidable: void); // Error
  (nVoidable: N); // Valid
}

if (typeof nVoidable === "number") {
  (nVoidable: void); // Error
  (nVoidable: N); // Valid
}

if (typeof nVoidable !== "number") {
  (nVoidable: void); // Valid
  (nVoidable: N); // Error
}

declare var nMaybe: ?N;

if (nMaybe == null) {
  (nMaybe: null | void); // Valid
  (nMaybe: N); // Error
}

if (nMaybe != null) {
  (nMaybe: null); // Error
  (nMaybe: void); // Error
  (nMaybe: N); // Valid
}

declare var nNumVoid: N | number | void;

if (typeof nNumVoid === "number") {
  (nNumVoid: void); // Error
  (nNumVoid: number); // Error
  (nNumVoid: N); // Error
  (nNumVoid: N | number); // Valid
}

if (typeof nNumVoid !== "number") {
  (nNumVoid: void); // Valid
  (nNumVoid: number); // Error
  (nNumVoid: N); // Error
  (nNumVoid: N | number); // Error
}

////////////
// string //
////////////
enum S of string {
  A = "",
  B = "B",
}

declare var sVoidable: void | S;

if (typeof sVoidable === "undefined") {
  (sVoidable: void); // Valid
  (sVoidable: S); // Error
}

if (typeof sVoidable !== "undefined") {
  (sVoidable: void); // Error
  (sVoidable: S); // Valid
}

if (typeof sVoidable === "string") {
  (sVoidable: void); // Error
  (sVoidable: S); // Valid
}

if (typeof sVoidable !== "string") {
  (sVoidable: void); // Valid
  (sVoidable: S); // Error
}

declare var sMaybe: ?S;

if (sMaybe == null) {
  (sMaybe: null | void); // Valid
  (sMaybe: S); // Error
}

if (sMaybe != null) {
  (sMaybe: null); // Error
  (sMaybe: void); // Error
  (sMaybe: S); // Valid
}

declare var sStrVoid: S | string | void;

if (typeof sStrVoid === "string") {
  (sStrVoid: void); // Error
  (sStrVoid: string); // Error
  (sStrVoid: S); // Error
  (sStrVoid: S | string); // Valid
}

if (typeof sStrVoid !== "string") {
  (sStrVoid: void); // Valid
  (sStrVoid: string); // Error
  (sStrVoid: S); // Error
  (sStrVoid: S | string); // Error
}

//////////////
// multiple //
//////////////
declare var bn: B | N;

if (typeof bn == "boolean") {
  (bn: B); // Valid
  (bn: N); // Error
}

if (typeof bn == "number") {
  (bn: B); // Error
  (bn: N); // Valid
}
