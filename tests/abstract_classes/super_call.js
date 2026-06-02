abstract class A {
  abstract m(): number;
  concrete(): number {
    return this.m();
  }
}

class B extends A {
  m(): number {
    return 1;
  }
  bad(): number {
    return super.m(); // ERROR
  }
}

class C extends A {
  m(): number {
    return 1;
  }
  good(): number {
    return super.concrete(); // OK
  }
}
