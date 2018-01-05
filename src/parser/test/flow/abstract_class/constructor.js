class         AbstractConstructor           { abstract        constructor(): void; } // Error, abstract constructor is not allowed
class         AbstractStaticConstructor     { abstract static constructor(): void; } // Error, abstract constructor is not allowed
declare class AbstractConstructorDecl       { abstract        constructor(): void; } // Error, abstract constructor is not allowed
declare class AbstractStaticConstructorDecl { abstract static constructor(): void; } // Error, abstract constructor is not allowed
