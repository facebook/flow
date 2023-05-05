// @flow

// computed property literal string initializers are not module refs
var o = {
  ['m#NotAModule']: null
};
