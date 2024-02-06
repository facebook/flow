import * as React from 'react';

hook useH(ref: React.RefObject<mixed>) {
    ref.current; // error
}

component Foo() {
    const x = React.useRef<mixed>();
    useH(x); // ok
    return null;
}
