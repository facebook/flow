class C {}
enum E {}

type T01 = {f:string};
type T02 = number;
type T03 = <A>(A) => void;
type T04 = Array<mixed>;
type T05 = C;
type T06 = empty;
type T07 = $ReadOnly<T01>;
type T08 = interface {};
type T09 = E;

declare var in_01: T01;
declare var in_02: T02;
declare var in_03: T03;
declare var in_04: T04;
declare var in_05: T05;
declare var in_06: T06;
declare var in_07: T07;
declare var in_08: T08;
declare var in_09: T09;

function test_01() {
  declare function grd(x: mixed): x is T01;
  if (grd(in_01) && grd(in_02) && grd(in_03) && grd(in_04) && grd(in_05) && grd(in_06) && grd(in_07) && grd(in_08) && grd(in_09)) {
    const obj = {in_01, in_02, in_03, in_04, in_05, in_06, in_07, in_08, in_09 };
//        ^
  }
}

function test_02() {
  declare function grd(x: mixed): x is T02;
  if (grd(in_01) && grd(in_02) && grd(in_03) && grd(in_04) && grd(in_05) && grd(in_06) && grd(in_07) && grd(in_08) && grd(in_09)) {
    const obj = {in_01, in_02, in_03, in_04, in_05, in_06, in_07, in_08, in_09 };
//        ^
  }
}

function test_03() {
  declare function grd(x: mixed): x is T03;
  if (grd(in_01) && grd(in_02) && grd(in_03) && grd(in_04) && grd(in_05) && grd(in_06) && grd(in_07) && grd(in_08) && grd(in_09)) {
    const obj = {in_01, in_02, in_03, in_04, in_05, in_06, in_07, in_08, in_09 };
//        ^
  }
}

function test_04() {
  declare function grd(x: mixed): x is T04;
  if (grd(in_01) && grd(in_02) && grd(in_03) && grd(in_04) && grd(in_05) && grd(in_06) && grd(in_07) && grd(in_08) && grd(in_09)) {
    const obj = {in_01, in_02, in_03, in_04, in_05, in_06, in_07, in_08, in_09 };
//        ^
  }
}

function test_05() {
  declare function grd(x: mixed): x is T05;
  if (grd(in_01) && grd(in_02) && grd(in_03) && grd(in_04) && grd(in_05) && grd(in_06) && grd(in_07) && grd(in_08) && grd(in_09)) {
    const obj = {in_01, in_02, in_03, in_04, in_05, in_06, in_07, in_08, in_09 };
//        ^
  }
}

function test_06() {
  declare function grd(x: mixed): x is T06;
  if (grd(in_01) && grd(in_02) && grd(in_03) && grd(in_04) && grd(in_05) && grd(in_06) && grd(in_07) && grd(in_08) && grd(in_09)) {
    const obj = {in_01, in_02, in_03, in_04, in_05, in_06, in_07, in_08, in_09 };
//        ^
  }
}

function test_07() {
  declare function grd(x: mixed): x is T07;
  if (grd(in_01) && grd(in_02) && grd(in_03) && grd(in_04) && grd(in_05) && grd(in_06) && grd(in_07) && grd(in_08) && grd(in_09)) {
    const obj = {in_01, in_02, in_03, in_04, in_05, in_06, in_07, in_08, in_09 };
//        ^
  }
}

function test_08() {
  declare function grd(x: mixed): x is T08;
  if (grd(in_01) && grd(in_02) && grd(in_03) && grd(in_04) && grd(in_05) && grd(in_06) && grd(in_07) && grd(in_08) && grd(in_09)) {
    const obj = {in_01, in_02, in_03, in_04, in_05, in_06, in_07, in_08, in_09 };
//        ^
  }
}

function test_09() {
  declare function grd(x: mixed): x is T09;
  if (grd(in_01) && grd(in_02) && grd(in_03) && grd(in_04) && grd(in_05) && grd(in_06) && grd(in_07) && grd(in_08) && grd(in_09)) {
    const obj = {in_01, in_02, in_03, in_04, in_05, in_06, in_07, in_08, in_09 };
//        ^
  }
}
