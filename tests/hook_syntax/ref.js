import * as React from 'react';

function useFakeHook(x: {+current: unknown}) { x.current }

component Component() {
    const x = React.useRef(null); // OK
    useFakeHook(x); // error both for calling a fake hook and for escaping ref
    return null;
}
