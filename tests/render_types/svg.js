import * as React from 'react';

declare const rendersSvg: renders 'svg'; // ok
declare const rendersDiv: renders 'div'; // error

<svg /> as renders 'svg'; // ok
<svg /> as renders? 'svg'; // ok
<svg /> as renders* 'svg'; // ok
<div /> as renders 'svg'; // error
<div /> as renders? 'svg'; // error
<div /> as renders* 'svg'; // error
rendersSvg as renders 'svg'; // ok
rendersSvg as renders? 'svg'; // ok
rendersSvg as renders* 'svg'; // ok
rendersDiv as renders 'svg'; // ok, rendersDiv is any

declare component Foo1() renders 'svg'; // ok
declare function Foo2(): renders 'svg'; // ok
<Foo1 /> as renders 'svg'; // ok
<Foo2 /> as renders 'svg'; // ok
declare component Bar();
<Bar /> as renders 'svg'; // error
