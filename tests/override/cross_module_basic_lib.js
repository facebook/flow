// Exports a base class for the cross-module override consumer.

export class Animal {
  makeSound(): string {
    return "...";
  }
  describe(): string {
    return "Animal that " + this.makeSound();
  }
}
