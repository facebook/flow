// @flow

interface D { }
declare class C<P> { };

declare class A extends C<any> { };

declare class G implements D { };

declare class B extends A mixins C<any> implements D { };

declare class E extends C<any> implements D { };

declare var typeof_C: typeof C;
() => typeof_C;

declare var class_of_C_number: Class<C<number>>;
() => class_of_C_number;
