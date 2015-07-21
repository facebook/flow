declare module M {
    export var x: number
    export class C {
        y : typeof x
    }
}

interface I {
    x : typeof M.x
    get_y(c: M.C): typeof M.x
}
