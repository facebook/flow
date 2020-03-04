// @flow

let tests = [
  function(attributes: NamedNodeMap) {
    attributes[0];
    attributes['data-testid'];

    // fails
    attributes[null];
    attributes[{}];
  }
];
