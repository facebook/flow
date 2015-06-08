declare module Module1 {
    export class A { }
    // If uncommented, the following throws an error
    /* declare module "M" {
        export = A;
    } */
}

// "Module2" is parsed as ExportModuleDeclaration { name : "Module2" , body }
declare module "Module2" {
    module M {
        export class A { }
    }
    export = M;
}
