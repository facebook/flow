// @flow

/*************** Basic functions ******************/
declare var x : { f : number};
declare var y : { y : string};

function foo(this : mixed) : void {
    return this.x; // error: this is mixed
}

foo(3); // error: foo does not take any arguments
foo.bind(x)();

function bar (this : {f : number, ...}) : number {
    this.y; // error no property y
    return this.f;
}
bar.bind(x)();
bar.bind(y)(); // error : number incompatible with string

/************** Object Methods ********************/

let o = {
    foo(this : {y : number, ...}) {}
}

o.foo(); // error o has no field y

let f = o.foo;
f.bind({y : 3})();

let o2 = {
    foo(this : {y : number , ...}) {},
    y : 3
}

o2.foo();
f.bind(o2)();

let o3 = {
    foo(this : {|y : number|}) {},
    y : 3
}

o3.foo(); // error, foo is missing in type of this
f.bind(o3)();
o3.foo.bind(o2)(); // error, foo is missing in type of o2 this

/******************* Generics **********************/

function baz<T> (this : { f : T ,... }) : T {
    return this.f;
}

(baz.bind({f : 3})() : number);
(baz.bind({f : ""})() : string);
(baz.bind({f : 3})() : string); // error : number incompatible with string
(baz.bind({f : ""})() : number); // error : number incompatible with string

declare function baz2<T>(this : T, x : ?T) : T;

(baz2.bind(3)(3) : number);
(baz2.bind(3)("") : string); // error : number incompatible with string
(baz2.bind(3)("") : number | string);
