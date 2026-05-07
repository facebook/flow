declare module M {
    declare module.exports: (x: string) => string;
}
declare module N {
    declare const x: number;
    declare const y: number;
    declare const z: number;
}
declare module Q {
    declare module.exports: $Exports<'M'>;
}
