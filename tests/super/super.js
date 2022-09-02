
class A {
  constructor(x:number) { }
  static staticMethod(x:string): string { return x; }
  f(x:string) { }
}

class B extends A {
  constructor(x:string,y:number) {
    super(x);
  }

  static anotherStatic() {
    (super.staticMethod('foo'): number); // error, string !~> number
    (super.doesntExist()); // error, A doesn't have a doesntExist method
  }

  g(): mixed {
    super.f(0);
    return super.g;
  }
}

class ExtendedByAssignSuper {
  x: number;
}

class AssignSuper extends ExtendedByAssignSuper {
  test() {
    super.x = 1;
  }
}
