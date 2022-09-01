// @flow

var container = class InstrumentedContainer extends container {
  foo(): mixed {
    return this.props;
  }
};
