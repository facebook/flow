//@flow
declare class A<T>{
  constructor(): A<T>
}
new A();

declare class B<+T>{
  constructor(T => mixed): B<T>;
}
new B((x: number) => 'string');

declare class C<-T>{
  constructor(T): C<T>;
}

new C(3);
