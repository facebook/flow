class         AsyncNamedMethod           { abstract        async(): void; }
declare class AsyncNamedMethodDecl       { abstract        async(): void; }
class         StaticAsyncNamedMethod     { abstract static async(): void; }
declare class StaticAsyncNamedMethodDecl { abstract static async(): void; }

class AbstractAsyncMethod       { abstract        async m(): void; }
class StaticAbstractAsyncMethod { abstract static async m(): void; }

class AbstractAsyncProperty       { abstract        async p: void; } //Error
class StaticAbstractAsyncProperty { abstract static async p: void; } //Error
