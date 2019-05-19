// @flow

import React from "react";

<button onClick={() => {}} />; // OK
<input onChange="foo" />; // Error: Expected function
<div onMouseMoveCapture={(event: SyntheticMouseEvent<HTMLElement>) => {}} />; // OK
<textarea onKeyDown={(event: SyntheticMouseEvent<HTMLElement>) => {}} />; // Error: Wrong event type
<p onKeyPressCapture={(event: SyntheticKeyboardEvent<HTMLElement>) => {}} />; // OK
<iframe onLoad={event => { (event.currentTarget: HTMLIFrameElement); }} />; // OK
<canvas onFocus={(event: SyntheticEvent<HTMLElement>) => {}} />; // OK to annotate loosely
<a onMouseEnter={(event: SyntheticMouseEvent<HTMLElement>) => {}} />; // OK
<nav onMouseLeave={(event: SyntheticMouseEvent<HTMLElement>) => {}} />; // OK
<span onTouchTapCapture={(event: SyntheticUIEvent<HTMLElement>) => {}} />; // OK
<dialog onCompositionEnd={(event: SyntheticCompositionEvent<HTMLElement>) => {}} />; // OK
<article onBeforeInput={(event: SyntheticInputEvent<HTMLElement>) => {}} />; // OK
<img onMouseDown={event => {
    const buttons: string = event.buttons; // Error: number ~> string
}} />