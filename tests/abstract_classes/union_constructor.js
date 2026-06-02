// `is_class_abstract` distributes through UnionT: a union of abstract
// and concrete class types is treated as any-abstract, so `new` is
// rejected because the runtime value may be the abstract branch.

abstract class AbstractA {}
class ConcreteB {}

declare const u: Class<AbstractA> | Class<ConcreteB>;
new u(); // ERROR: union may resolve to AbstractA at runtime
