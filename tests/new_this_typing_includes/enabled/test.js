// @flow

class Enabled {
  static noThis(): void {}

  static usesThis(): typeof Enabled { // error: need to annotate this
    return this;
  }
}

const extracted: () => void = Enabled.noThis; // ok
