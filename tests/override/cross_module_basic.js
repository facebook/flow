// The inherited-name walk must follow `extends` across module boundaries.

import {Animal} from './cross_module_basic_lib';

class Dog extends Animal {
  override makeSound(): string { // OK: inherited from imported Animal
    return "barks";
  }
}

class Cat extends Animal {
  override purr(): void {} // ERROR: `purr` is not declared in `Animal`
}

new Dog().makeSound() as string; // OK
