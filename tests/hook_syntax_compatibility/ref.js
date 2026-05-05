import * as React from 'react';

function useFakeHook(x: {+current: unknown}) { x.current }

component Component() {
    const x = React.useRef(null);
    useFakeHook(x); // ok, because compatibility
    return null;
}
