class MyClass {
  x: number = 1;
  y: string = "hello";

  method(): typeof this {
    return this;
  }

  getX(): typeof this.x {
    return this.x;
  }
}

declare const obj: MyClass;
obj.method() as empty; // ERROR
obj.getX() as empty; // ERROR
