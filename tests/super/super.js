
class A {
  constructor(x:number) { }
  f(x:string) { }
}

class B extends A {
  constructor(x:string,y:number) {
    super(x);
  }
  g() {
    super.f(0);
    return super.g;
  }
}
