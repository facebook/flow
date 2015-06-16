declare module M {
    export var x: number
    export class C {
        y : typeof x
    }
    export var m : typeof N.z
    export module N {
        export var z : string
    }
}

declare module P {
    export var a : typeof M.N.z
}
