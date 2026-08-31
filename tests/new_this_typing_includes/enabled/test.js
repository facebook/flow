// @flow

class Enabled {
  static noThis(): void {}

  static usesThis(): typeof Enabled { // ok
    return this;
  }
}

const extracted: () => void = Enabled.noThis; // error
