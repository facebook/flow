import * as React from 'react';
import * as HookReact from './hookReact';

component Foo() {
    const x = React.useRef(null); // error, because currently useRef is a function not a hook
    return null;
}

function Bar(props: mixed) {
    const x = HookReact.useRef(null); // error, because this useRef is a hook and we're in a function
    return null;
}
