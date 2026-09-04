// @flow

class A {
  #f: number = 1;
  #m(): void {}
  get #g(): string { return ''; }
  static #sf: boolean = true;

  use(): void {
    this.#m();
    this.#f;
    this.#g;
    A.#sf;
  }
}

class B {
  #f: string = '';
  use(): string {
    return this.#f;
  }
}
