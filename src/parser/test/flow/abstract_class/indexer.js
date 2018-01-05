class AbstractIndexer                   { abstract        [string]: void; } //Error, error_unexpected on `[`, abstract properties not allowed is ok too
class StaticAbstractIndexer             { abstract static [string]: void; } //Error
declare class AbstractIndexerDecl       { abstract        [string]: void; } //Error
declare class StaticAbstractIndexerDecl { abstract static [string]: void; } //Error
