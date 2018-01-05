class         AbstractGetMethod           { abstract        get(): void; }
class         AbstractStaticGetMethod     { abstract static get(): void; }
declare class AbstractGetMethodDecl       { abstract        get(): void; }
declare class AbstractStaticGetMethodDecl { abstract static get(): void; }

class         AbstractGetter           { abstract        get p(): void {} } //Error
class         AbstractStaticGetter     { abstract static get p(): void {} } //Error
declare class AbstractGetterDecl       { abstract        get p(): void;   } //Error
declare class AbstractStaticGetterDecl { abstract static get p(): void;   } //Error

class         GetterNamedAbstract           {        get abstract(): void {} }
class         StaticGetterNamedAbstract     { static get abstract(): void {} }
declare class GetterNamedAbstractDecl       {        get abstract(): void;   }
declare class StaticGetterNamedAbstractDecl { static get abstract(): void;   }
