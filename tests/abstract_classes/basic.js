abstract class Animal {
  abstract name: string;
  abstract makeSound(): string;

  describe(): string {
    return this.name + " says " + this.makeSound();
  }
}

((a: Animal) => {
  a.makeSound() as string; // OK
});

new Animal(); // ERROR

const ctor = Animal as Class<Animal>; // OK
new ctor(); // ERROR
