declare module R {
    module T {
        export module U {

        }
        export class E { }
    }
    export class A extends C { }
    export interface I { }
    var x : C
    var y : T.E
}

declare module S {
   // export class CC extends R.A { }
    var z : typeof R.x
}

declare module SS {
   // export interface II extends R.I { }
}
