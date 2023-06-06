component Bar() {
    this; // error
}

component Baz() {
    class Nested {
        m() {
            this; // ok
        }
    }
}

import * as React from 'react';

class Ext {
    x = 42;
    m() {
        component Foo(x: string = this.x) { } // error from num ~> string, no error from this
        <Foo />; // ok
    }
}
