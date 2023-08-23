import * as React from 'react';

{
    component NoReturn() { } // error
}

{
    component HasReturn() { return <div />;} // no error
}

{
    component Throws() {
        throw new Error(); // no error
    }

    component Throws2(f: boolean) { // error
        if (f) {
            throw new Error();
        } else {

        }
    }
}

{
    type A = "Foo" | "Bar";

    component C() { return <div />}

    component NotExhaustive(t: A) { // error
        switch (t) {
            case "Foo":
                return <C />
        }
    }

    component Exhaustive1(t: A) { // no error
        switch (t) {
            case "Foo":
                return <C />
            case "Bar":
                return <C />
        }
    }

    component Exhaustive2(t: A) { // no error
        switch (t) {
            case "Foo":
                return <C />
            default:
                return <C />
        }
    }
}

{
  component C(x: number, b: boolean) { // ERROR
    switch (x) {
      default:
        if (b) break;
        return <div />;
    }
  }
}
