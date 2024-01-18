declare module 'c' {
    declare module.exports: number; // ok
    declare module.exports: string; // error
    declare module.exports: boolean; // error
}

declare module 'd' {
    declare export var a: number;
    declare module.exports: number; // error
}
