// Exports a basic abstract class and a concrete subclass for cross-module
// consumer tests.

export abstract class Animal {
  abstract makeSound(): string;

  describe(): string {
    return "Animal that " + this.makeSound();
  }
}

export class Dog extends Animal {
  makeSound(): string {
    return "barks";
  }
}
