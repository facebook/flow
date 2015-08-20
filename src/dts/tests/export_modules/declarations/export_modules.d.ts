declare module "module-P" {
    class D { }
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
    export = P
}
