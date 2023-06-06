// Components uses React$AbstractComponent statics
component Foo() { return (42: any); }

Foo.displayName = 3; // ERROR
Foo.displayName = 'str'; // OK
Foo.randomProperty; // ERROR!

module.exports = { Foo };
