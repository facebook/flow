declare module M {
    export var x: number
    export var y: string
    export var z: string
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

declare module F {
    export function successor(n: typeof M.x) : typeof M.x
}

declare module T {
    type AnyThing = number | string | M.C
    export function identity(x: AnyThing) : AnyThing
}
