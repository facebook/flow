abstract class Animal {
  abstract name: string;
  abstract makeSound(): string;
}

class Dog extends Animal {
  name: string = "d";
  makeSound(): string {
    return "woof";
  }
}
new Dog().makeSound() as string; // OK

class Cat extends Animal { // ERROR
  name: string = "c";
}

class Mute extends Animal { // ERROR
  makeSound(): string {
    return "";
  }
}

abstract class Stub extends Animal {}
new Stub(); // ERROR

abstract class A {
  abstract a(): number;
}
abstract class B extends A {
  abstract b(): number;
}
class C extends B { // ERROR
  a(): number {
    return 1;
  }
}
