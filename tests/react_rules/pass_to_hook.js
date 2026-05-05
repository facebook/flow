import * as React from 'react';

hook useH(ref: React.RefObject<unknown>) {
    ref.current; // error
}

component Foo() {
    const x = React.useRef<unknown>();
    useH(x); // ok
    return null;
}
