component Comp() {} // error: unsupported

// But fragments are still using component syntax and are not any:
(3: React$FragmentType); // ERROR
module.exports = { Comp };

declare component Foo() renders Foo; // ok
Foo as empty; // ERROR

declare const c: component(); // ok
c as empty; // ERROR
