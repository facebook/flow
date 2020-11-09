//@flow

class C {
  +f: () => this;
  +f2: () => C;
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
