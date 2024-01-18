import * as React from 'react';

component Foo() {
    const x = React.useRef(null); // error, because currently useRef is a function not a hook
    return null;
}
