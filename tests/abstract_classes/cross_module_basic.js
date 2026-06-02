// The `abstract` bit on a class must survive crossing a module boundary:
// importing an abstract class still rejects `new`, requires subclasses to
// implement abstract members, and exposes inherited concrete methods on
// instances of concrete subclasses.

import {Animal, Dog} from './cross_module_basic_lib';

new Animal(); // ERROR: imported abstract class is still abstract

class Cat extends Animal {} // ERROR: missing `makeSound`

class Fox extends Animal { // OK: implements `makeSound`
  makeSound(): string {
    return "yips";
  }
}

new Fox().makeSound() as string; // OK
new Fox().describe() as string; // OK: inherited concrete method
new Dog().describe() as string; // OK
