class AbstractPrivateMethod                   { abstract        #m(): void; } //Error
declare class AbstractPrivateMethodDecl       { abstract        #m(): void; } //Error
class StaticAbstractPrivateMethod             { abstract static #m(): void; } //Error
declare class StaticAbstractPrivateMethodDecl { abstract static #m(): void; } //Error

class AbstractPrivateProperty                   { abstract        #m: void; } //Error
declare class AbstractPrivatePropertyDecl       { abstract        #m: void; } //Error
class StaticAbstractPrivateProperty             { abstract static #m: void; } //Error
declare class StaticAbstractPrivatePropertyDecl { abstract static #m: void; } //Error
