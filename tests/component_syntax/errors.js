component Bar() {
    this; // error
    return (42: any);
}

component Baz() {
    class Nested {
        m() {
            this; // ok
        }
    }
    return (42: any)
}

import * as React from 'react';

class Ext {
    x = 42;
    m() {
        component Foo(x: string = this.x) { } // error from num ~> string, no error from this
        <Foo />; // ok
    }
}

component lowercase() { return (42: any) } // error
