class A {
    constructor() { this.x = 0; this._x = ""; this.__x = 0; }
}

class B extends A {
    foo() {
        var x:number = this.x;
        var _x:string = this._x;
        var __x:number = this.__x;
    }
}
