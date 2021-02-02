//@flow
declare class A<+T>{
  constructor(T): A<T>;
}
new A(3);
new A((x: number) => 'string');


declare class B<+T>{
  constructor(T, T): B<T>;
}
new B(3, 'string');
new B('string', (x: number) => 'string');
