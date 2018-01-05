class         AbstractSetMethod           { abstract        set(p: void): void; }
class         AbstractStaticSetMethod     { abstract static set(p: void): void; }
declare class AbstractSetMethodDecl       { abstract        set(p: void): void; }
declare class AbstractStaticSetMethodDecl { abstract static set(p: void): void; }

class         AbstractSetter           { abstract        set p(p: void): void {} } //Error
class         AbstractStaticSetter     { abstract static set p(p: void): void {} } //Error
declare class AbstractSetterDecl       { abstract        set p(p: void): void;   } //Error
declare class AbstractStaticSetterDecl { abstract static set p(p: void): void;   } //Error

class         SetterNamedAbstract           {        set abstract(p: void): void {} }
class         StaticSetterNamedAbstract     { static set abstract(p: void): void {} }
declare class SetterNamedAbstractDecl       {        set abstract(p: void): void;   }
declare class StaticSetterNamedAbstractDecl { static set abstract(p: void): void;   }
