import * as React from 'react';

function useFakeHook(x: {+current: mixed}) { x.current }

component Component() {
    const x = React.useRef(null); // unrelated error due to useRef not actually being a hook currently
    useFakeHook(x); // error both for calling a fake hook and for escaping ref
    return null;
}
