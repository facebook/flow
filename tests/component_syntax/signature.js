//@flow
import * as React from 'react';

component InlineOnly(
  foo: number,
  bar: string,
) {
  return null;
}

const inlineOnlyGood = <InlineOnly foo={3} bar="" />; // OK!
const inlineOnlyBad = <InlineOnly foo="" bar={3} />; // Error 2x

component InexactRest(
  foo: number,
  ...rest: {bar: string, ...}
) {
  return null;
}

const inexactGood = <InexactRest foo={3} bar="str" baz={3} />; // OK!
const inexactBad = <InexactRest foo="" bar={3} baz={3} />; // ERROR 2x for foo and bar

component IndexedRest(
  foo: number,
  ...rest: {[string]: number, ...}
) {
  return null;
}

const indexedGood = <IndexedRest foo={3} bar={3} />; // OK!
const indexedBad = <IndexedRest foo="" bar="str" />; // ERROR 2x for foo and bar

component DefaultProps(
  foo: number = 3,
) {
  return null;
}

const defaultsGood = <DefaultProps />; // OK!
const defaultsBad = <DefaultProps foo="bad" />; // ERROR

component RenamedParams(
  foo as bar: number,
  bar as foo: string,
  'non-ident' as baz: string,
) {}

const renamedGood = <RenamedParams foo={3} bar="str" non-ident="str" />; // OK!
const renamedWrongProps = <RenamedParams bar={3} foo="str" baz="str" />; // ERROR 4x, bad foo type, bad bar type, no non-ident, extra baz
