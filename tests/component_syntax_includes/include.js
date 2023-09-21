export component Foo() { return null }
declare export component Bar(x: number);
export const Baz: component() = (42: any);

component WriteProps(prop: { foo: number} ) {
    prop.foo = 42;
    return null;
}
