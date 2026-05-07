declare const any: any;

function Foo(props: {}) {}

(any as React.ElementRef<typeof Foo>).nope; // Error
