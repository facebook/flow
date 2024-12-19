// @flow
var bar = 123;

class C {
    boo = "";

    baz() {
        let C1 = class {
            bye = "";

            been() {

            }

            qux() {
                b
//               ^
            }
        }
    }

    foo() {
        b
//       ^
    }
}

let C2 = class {
    boo = "";

    baz() {

    }

    foo() {
        b
//       ^
    }
}
