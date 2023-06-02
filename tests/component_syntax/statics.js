// Components uses React$AbstractComponent statics
component Foo() {}

Foo.displayName = 3; // ERROR
Foo.displayName = 'str'; // OK
Foo.randomProperty; // ERROR!

module.exports = { Foo };
