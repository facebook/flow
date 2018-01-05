class         LeadingMethod       { m(): void {} abstract m(): void; } // Error
declare class LeadingMethodDecl   { m(): void;   abstract m(): void; } // Error
class         LeadingProperty     { m:   void;   abstract m(): void; } // Error
declare class LeadingPropertyDecl { m:   void;   abstract m(): void; } // Error

class         TrailingMethod       { abstract m(): void; m(): void {} } // Error
declare class TrailingMethodDecl   { abstract m(): void; m(): void;   } // Error
class         TrailingProperty     { abstract m(): void; m:   void;   } // Error
declare class TrailingPropertyDecl { abstract m(): void; m:   void;   } // Error
