//@flow
declare class A<T>{
  constructor(T): A<T>;
}
new A(3);
new A((x: number) => 'string');
