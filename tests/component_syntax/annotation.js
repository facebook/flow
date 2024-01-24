import * as React from 'react';

declare var Basic: component();
<Basic />; // ok
<Basic foo={42} /> // error

declare var InlineOnly: component(
  foo: number,
  bar: string,
);

const inlineOnlyGood = <InlineOnly foo={3} bar="" />; // OK!
const inlineOnlyBad = <InlineOnly foo="" bar={3} />; // Error 2x

declare var InexactRest: component(
  foo: number,
  ...rest: {bar: string, ...}
);

const inexactGood = <InexactRest foo={3} bar="str" baz={3} />; // OK!
const inexactBad = <InexactRest foo="" bar={3} baz={3} />; // ERROR 2x for foo and bar

declare var IndexedRest: component(
  foo: number,
  ...rest: {[string]: number, ...}
);

const indexedGood = <IndexedRest foo={3} bar={3} />; // OK!
const indexedBad = <IndexedRest foo="" bar="str" />; // ERROR 2x for foo and bar

declare var DefaultProps: component(
  foo?: number,
);

const defaultsGood = <DefaultProps />; // OK!
const defaultsBad = <DefaultProps foo="bad" />; // ERROR

declare var OptionalRest: component(
    ...{foo?: number}
  );

const optGood = <OptionalRest />; // OK!
const optBad = <OptionalRest foo="bad" />; // ERROR

declare var Duplicate: component(x: number, ...{x: number}); // error
<Duplicate x={1} />;

declare var InlineRef: component(ref: number); // error
<InlineRef ref={1} />;

declare var SpreadRef: component(...p: {ref: number}); // error
<SpreadRef ref={1} />;

declare export var Export: component(x: number) renders number; // invalid-render

declare var DefaultExport: component(x: number);
export default DefaultExport;

<Export />; // error
<DefaultExport x={"a"} />; // error
