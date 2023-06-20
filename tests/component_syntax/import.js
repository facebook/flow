import { InlineOnly, InexactRest, IndexedRest, DefaultProps, RenamedParams } from "./signature.js"
import * as React from "react"

const inlineOnlyGood = <InlineOnly foo={3} bar="" />; // OK!
const inlineOnlyBad = <InlineOnly foo="" bar={3} />; // Error 2x

const inexactGood = <InexactRest foo={3} bar="str" baz={3} />; // OK!
const inexactBad = <InexactRest foo="" bar={3} baz={3} />; // ERROR 2x for foo and bar

const indexedGood = <IndexedRest foo={3} bar={3} />; // OK!
const indexedBad = <IndexedRest foo="" bar="str" />; // ERROR 2x for foo and bar

const defaultsGood = <DefaultProps />; // OK!
const defaultsBad = <DefaultProps foo="bad" />; // ERROR

const renamedGood = <RenamedParams foo={3} bar="str" non-ident="str" />; // OK!
const renamedWrongProps = <RenamedParams bar={3} foo="str" baz="str" />; // ERROR 4x, bad foo type, bad bar type, no non-ident, extra baz

import { Foo } from "./statics.js"
<Foo />; // Ok
(Foo.displayName: empty); // error, displayName is a string
Foo.randomProperty; // ERROR!

import { C } from "./export_syntax.js"
import D from "./export_syntax.js"

<C x={"a"} />; // error
<D y={"a"} /> // 2 errors

import { Export } from "./declared.js";
import DefaultExport from "./declared.js";

<Export />; // error
<DefaultExport x={"a"} />; // error

import { NoRef }from "./names.js";
<NoRef /> // no error
