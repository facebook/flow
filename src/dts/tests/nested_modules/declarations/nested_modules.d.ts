declare module M {
    export var x: number
    export class C {
        y : typeof P.b
    }
    export var m : typeof N.z
    export module N {
        export var z : typeof x
    }
}

declare module P {
    export var a : typeof M.N.z
    export var b : number
}
