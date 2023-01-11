//@flow
declare class A<T>{
  constructor(): A<T>
}
new A(); // Error: T underconstrained

declare class B<+T>{
  constructor(T => mixed): B<T>;
}
new B((x: number) => 'string'); // Ok: It will error under Pierce, but we also consider upper bounds here.

declare class C<-T>{
  constructor(T): C<T>;
}

new C(3); // Ok: It will error under Pierce, but we also consider lower bounds here.

new Array(1); // Error
