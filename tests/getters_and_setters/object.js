/**
 * @flow
 */

var z: number = 123;

class A {}
class B extends A {}
class C extends A {}

var obj = {
  get goodGetterWithAnnotation(): number { return 4; },

  set goodSetterWithAnnotation(x: number) { z = x; },

  get propWithMatchingGetterAndSetter(): number { return 4; },
  set propWithMatchingGetterAndSetter(x: number) { },

  // The getter and setter need not have the same type
  get propWithSubtypingGetterAndSetter(): ?number { return 4; }, // OK
  set propWithSubtypingGetterAndSetter(x: number) { },

  set propWithSubtypingGetterAndSetterReordered(x: number) { }, // OK
  get propWithSubtypingGetterAndSetterReordered(): ?number { return 4; },

  get exampleOfOrderOfGetterAndSetter(): A { return new A(); },
  set exampleOfOrderOfGetterAndSetter(x: B) {},

  set exampleOfOrderOfGetterAndSetterReordered(x: B) {},
  get exampleOfOrderOfGetterAndSetterReordered(): A { return new A(); },

  set [z](x: string) {},
  get [z](): string { return string; },

  set "stringLiteral"(x: number) { },
  get "stringLiteral"(): number { return 4; },
};



// Test getting properties with getters
var testGetterNoError: number = obj.goodGetterWithAnnotation;
var testGetterWithError: string = obj.goodGetterWithAnnotation; // Error number ~> string

// Test setting properties with setters
obj.goodSetterWithAnnotation = 123;
obj.goodSetterWithAnnotation = "hello"; // Error string ~> number

var testSubtypingGetterAndSetter: number = obj.propWithSubtypingGetterAndSetter; // Error ?number ~> number

// When building this feature, it was tempting to flow the setter into the
// getter and then use either the getter or setter as the type of the property.
// This example shows the danger of using the getter's type
obj.exampleOfOrderOfGetterAndSetter = new C(); // Error C ~> B

// And this example shows the danger of using the setter's type.
var testExampleOrOrderOfGetterAndSetterReordered: number =
  obj.exampleOfOrderOfGetterAndSetterReordered; // Error A ~> B

(obj["stringLiteral"]: string); // err, num !~> string
obj["stringLiteral"] = "foo"; // err, string !~> num
