declare module 'c' {
    declare module.exports: number; // ok
    declare module.exports: string; // ok
    declare module.exports: boolean; // ok
}

declare module 'd' {
    declare export var a: number;
    declare module.exports: number; // error
}
