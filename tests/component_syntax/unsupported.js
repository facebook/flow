// Even when components are enabled by options, the implementation is not far
// enough along for it to be safely used. We error on all uses for now.

component Foo() { return (42: any) } // ERROR
