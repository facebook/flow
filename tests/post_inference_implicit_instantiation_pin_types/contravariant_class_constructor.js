//@flow
declare class A<-T> {
  constructor (T => mixed): A<T>;
}
new A((x: number) => 'string');


declare class B<-T> {
  constructor(T => mixed, T => mixed): B<T>;
} 
new B((x: string) => 3, (x: number) => 'string');
