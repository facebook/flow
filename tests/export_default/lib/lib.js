declare module M {
    declare function exports(x:string): string;
}
declare module N {
    declare var x: number;
}
declare module Q {
    declare var exports: $Exports<'M'>;
}
