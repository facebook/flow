var container = class InstrumentedContainer extends container {
  foo(): unknown {
    return this.props;
  }
};
