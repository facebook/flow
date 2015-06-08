declare module Module1 {
    export class A { }
}

// "Module2" is parsed as ExportModuleDeclaration { name : "Module2" , body }
declare module "Module2" {
    export = Module1;
}
