// Mixin class extends - error persists even with tslib_syntax enabled
declare function Mixin<T>(base: T): T;
declare class Base {}
class MixinTest extends Mixin(Base) {} // ERROR
declare class DeclMixinTest extends Mixin(Base) {} // ERROR
