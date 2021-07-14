//@flow
function foo() {return this.x;}

class A {
    x : number;
    static y : number;
    foo() {return this.x;}
    static bar() {return this.y;}
}

let z = () => { return this };

let o = {
    // $FlowExpectedError[object-this-reference]
    foo() { return this; }
}

let f = function () { return this.x }
