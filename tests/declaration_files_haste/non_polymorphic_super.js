class C {}
class D<T> extends C<T> {} // ERROR

class Foo {}
declare var B: Class<Foo>;
class E<T> extends B<T> {} // ERROR
