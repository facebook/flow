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
  bVoidable as void; // Valid
  bVoidable as B; // Error
}

if (typeof bVoidable !== "undefined") {
  bVoidable as void; // Error
  bVoidable as B; // Valid
}

if (typeof bVoidable === "boolean") {
  bVoidable as void; // Error
  bVoidable as B; // Valid
}

if (typeof bVoidable !== "boolean") {
  bVoidable as void; // Valid
  bVoidable as B; // Error
}

if (bVoidable === undefined) {
  bVoidable as void; // Valid
  bVoidable as B; // Error
}

if (bVoidable !== undefined) {
  bVoidable as void; // Error
  bVoidable as B; // Valid
}

declare var bMaybe: ?B;

if (bMaybe == null) {
  bMaybe as null | void; // Valid
  bMaybe as B; // Error
}

if (bMaybe != null) {
  bMaybe as null; // Error
  bMaybe as void; // Error
  bMaybe as B; // Valid
}

if (bMaybe === null || bMaybe === undefined) {
  bMaybe as null | void; // Valid
  bMaybe as B; // Error
}

if (bMaybe !== null && bMaybe !== undefined) {
  bMaybe as null; // Error
  bMaybe as void; // Error
  bMaybe as B; // Valid
}

declare var bBoolVoid: B | boolean | void;

if (typeof bBoolVoid === "boolean") {
  bBoolVoid as void; // Error
  bBoolVoid as boolean; // Valid
  bBoolVoid as B; // Error
  bBoolVoid as B | boolean; // Valid
}

if (typeof bBoolVoid !== "boolean") {
  bBoolVoid as void; // Valid
  bBoolVoid as boolean; // Error
  bBoolVoid as B; // Error
  bBoolVoid as B | boolean; // Error
}

if (bVoidable) {
  bVoidable as void; // Error
  bVoidable as B; // Valid
}

if (!bVoidable) {
  bVoidable as void | B; // Valid
  bVoidable as B; // Error
  bVoidable as void; // Error
}

enum BEmpty {}
declare var bEmpty: BEmpty | void;

if (bEmpty) {
  bEmpty as void; // Error
  bEmpty as BEmpty; // Valid
}

if (!bEmpty) {
  bEmpty as void; // Valid
  bEmpty as BEmpty; // Error
}

enum BTrue {
  A = true,
}
declare var bTrue: BTrue | void;

if (bTrue) {
  bTrue as void; // Error
  bTrue as BTrue; // Valid
}

if (!bTrue) {
  bTrue as void; // Valid
  bTrue as BTrue; // Error
}

enum BFalse {
  A = false,
}
declare var bFalse: BFalse | true;

if (bFalse) {
  bFalse as true; // Valid
  bFalse as BFalse; // Error
}

if (!bFalse) {
  bFalse as true; // Error
  bFalse as BFalse; // Valid
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
  nVoidable as void; // Valid
  nVoidable as N; // Error
}

if (typeof nVoidable !== "undefined") {
  nVoidable as void; // Error
  nVoidable as N; // Valid
}

if (typeof nVoidable === "number") {
  nVoidable as void; // Error
  nVoidable as N; // Valid
}

if (typeof nVoidable !== "number") {
  nVoidable as void; // Valid
  nVoidable as N; // Error
}

declare var nMaybe: ?N;

if (nMaybe == null) {
  nMaybe as null | void; // Valid
  nMaybe as N; // Error
}

if (nMaybe != null) {
  nMaybe as null; // Error
  nMaybe as void; // Error
  nMaybe as N; // Valid
}

declare var nNumVoid: N | number | void;

if (typeof nNumVoid === "number") {
  nNumVoid as void; // Error
  nNumVoid as number; // Valid
  nNumVoid as N; // Error
  nNumVoid as N | number; // Valid
}

if (typeof nNumVoid !== "number") {
  nNumVoid as void; // Valid
  nNumVoid as number; // Error
  nNumVoid as N; // Error
  nNumVoid as N | number; // Error
}

if (nVoidable) {
  nVoidable as void; // Error
  nVoidable as N; // Valid
}

if (!nVoidable) {
  nVoidable as void | N; // Valid
  nVoidable as N; // Error
  nVoidable as void; // Error
}

enum NTruthy {
  A = 1,
  B = 2,
}
declare var nTruthy: NTruthy | void;

if (nTruthy) {
  nTruthy as void; // Error
  nTruthy as NTruthy; // Valid
}

if (!nTruthy) {
  nTruthy as void; // Valid
  nTruthy as NTruthy; // Error
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
  sVoidable as void; // Valid
  sVoidable as S; // Error
}

if (typeof sVoidable !== "undefined") {
  sVoidable as void; // Error
  sVoidable as S; // Valid
}

if (typeof sVoidable === "string") {
  sVoidable as void; // Error
  sVoidable as S; // Valid
}

if (typeof sVoidable !== "string") {
  sVoidable as void; // Valid
  sVoidable as S; // Error
}

declare var sMaybe: ?S;

if (sMaybe == null) {
  sMaybe as null | void; // Valid
  sMaybe as S; // Error
}

if (sMaybe != null) {
  sMaybe as null; // Error
  sMaybe as void; // Error
  sMaybe as S; // Valid
}

declare var sStrVoid: S | string | void;

if (typeof sStrVoid === "string") {
  sStrVoid as void; // Error
  sStrVoid as string; // Valid
  sStrVoid as S; // Error
  sStrVoid as S | string; // Valid
}

if (typeof sStrVoid !== "string") {
  sStrVoid as void; // Valid
  sStrVoid as string; // Error
  sStrVoid as S; // Error
  sStrVoid as S | string; // Error
}

if (sVoidable) {
  sVoidable as void; // Error
  sVoidable as S; // Valid
}

if (!sVoidable) {
  sVoidable as void | S; // Valid
  sVoidable as S; // Error
  sVoidable as void; // Error
}

enum STruthy {
  A,
  B,
}
declare var sTruthy: STruthy | void;

if (sTruthy) {
  sTruthy as void; // Error
  sTruthy as STruthy; // Valid
}

if (!sTruthy) {
  sTruthy as void; // Valid
  sTruthy as STruthy; // Error
}

//////////////
// multiple //
//////////////
declare var bn: B | N;

if (typeof bn == "boolean") {
  bn as B; // Valid
  bn as N; // Error
}

if (typeof bn == "number") {
  bn as B; // Error
  bn as N; // Valid
}

//////////////////
// sketchy-null //
//////////////////
// flowlint sketchy-null:error

if (bMaybe) { } // Error
if (!bMaybe) { } // Error

if (nMaybe) { } // Error
if (!nMaybe) { } // Error

if (sMaybe) { } // Error
if (!sMaybe) { } // Error

if (bTrue) { } // Valid
if (!bTrue) { } // Valid

if (nTruthy) { } // Valid
if (!nTruthy) { } // Valid

if (sTruthy) { } // Valid
if (!sTruthy) { } // Valid

// flowlint sketchy-null:off

////////////
// bigint //
////////////
enum Big of bigint {
  A = 0n,
  B = 1n,
}

declare var bigVoidable: void | Big;

if (typeof bigVoidable === "undefined") {
  bigVoidable as void; // Valid
  bigVoidable as Big; // Error
}

if (typeof bigVoidable !== "undefined") {
  bigVoidable as void; // Error
  bigVoidable as Big; // Valid
}

if (typeof bigVoidable === "bigint") {
  bigVoidable as void; // Error
  bigVoidable as Big; // Valid
}

if (typeof bigVoidable !== "bigint") {
  bigVoidable as void; // Valid
  bigVoidable as Big; // Error
}

declare var bigMaybe: ?Big;

if (bigMaybe == null) {
  bigMaybe as null | void; // Valid
  bigMaybe as Big; // Error
}

if (bigMaybe != null) {
  bigMaybe as null; // Error
  bigMaybe as void; // Error
  bigMaybe as Big; // Valid
}

declare var bigBigintVoid: Big | bigint | void;

if (typeof bigBigintVoid === "bigint") {
  bigBigintVoid as void; // Error
  bigBigintVoid as bigint; // Valid
  bigBigintVoid as Big; // Error
  bigBigintVoid as Big | bigint; // Valid
}

if (typeof bigBigintVoid !== "bigint") {
  bigBigintVoid as void; // Valid
  bigBigintVoid as bigint; // Error
  bigBigintVoid as Big; // Error
  bigBigintVoid as Big | bigint; // Error
}

if (bigVoidable) {
  bigVoidable as void; // Error
  bigVoidable as Big; // Valid
}

if (!bigVoidable) {
  bigVoidable as void | Big; // Valid
  bigVoidable as Big; // Error
  bigVoidable as void; // Error
}

enum BigTruthy {
  A = 1n,
  B = 2n,
}
declare var bigTruthy: BigTruthy | void;

if (bigTruthy) {
  bigTruthy as void; // Error
  bigTruthy as BigTruthy; // Valid
}

if (!bigTruthy) {
  bigTruthy as void; // Valid
  bigTruthy as BigTruthy; // Error
}
