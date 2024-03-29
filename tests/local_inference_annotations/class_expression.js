//@flow
const x: any = class A {
  method(param): any { // error on param, annotation is not pushed down
    (x) => 3; // error
    return (x) => 3; // ok
  }

  property1 = (x) => 3; // error
  property2: any = (x) => 3; //ok

  methodBody(annotatedParam: any) { // ok
    const x: any = (x) => 3; // ok
  }

  annotatedMethod(param: any): any {}
};
