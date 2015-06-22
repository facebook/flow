declare module P {
    export var x: number
    export class C {
        y : typeof x
    }
    export var m : typeof N.z
    export module N {
        export var z : typeof x
    }
}

declare module "module-P" {
    export = P
}
