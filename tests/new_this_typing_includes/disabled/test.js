// @flow

class Disabled {
  static noThis(): void {}

  static usesThis(): typeof Disabled { // okay: legacy behavior outside the glob
    return this;
  }
}

const extracted: () => void = Disabled.noThis; // error: legacy method unbinding
