class Duplicate           { abstract        m(): void; abstract        m(): void; } //Error
class StaticDuplicate     { abstract static m(): void; abstract static m(): void; } //Error
class DuplicateDecl       { abstract        m(): void; abstract        m(): void; } //Error
class StaticDuplicateDecl { abstract static m(): void; abstract static m(): void; } //Error
