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

component HelloWorld(unannotated) { return } // error

component Outer() {
    component Inner () { return null; } // lint error
    function f() {
        component FunctionNested() { return null; } // ok
    }
    class C {
        m(): void {
            component ClassNested() { return null; } // ok
        }
    }
    if (true) {
        component IfNested() { return null; } // lint error
    }
    return null;
}
