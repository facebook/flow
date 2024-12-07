declare export component Foo(bar: string, baz: number) renders Base; // incompatible-type with baz prop, otherwise ok
declare component Base();
declare component Acid();

declare export component DifferentRenders() renders Acid; // error: incompatible renders type
