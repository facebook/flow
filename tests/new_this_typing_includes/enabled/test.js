// @flow

class Enabled {
  static noThis(): void {}

  static usesThis(): typeof Enabled { // okay: `this` is inferred as `typeof Enabled`
    return this;
  }
}

const extracted: () => void = Enabled.noThis; // ok
const unboundUsesThis = Enabled.usesThis;
unboundUsesThis(); // error: the class receiver is missing
