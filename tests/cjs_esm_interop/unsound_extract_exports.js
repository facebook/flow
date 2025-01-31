class Foo {
  #v: string = '';

  foo(): string {
    return this.#v;
  }
}

const instance: Foo = new Foo();
module.exports = instance;
