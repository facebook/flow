//@flow

class C {
  readonly f: () => this;
  readonly f2: () => C;
  g(): this {
    if (C) {
      return this.f2();
    } // nope
    return this.f();
  }
  clone(): this {
    return new this.constructor();
  }
}
