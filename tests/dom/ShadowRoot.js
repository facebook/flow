// @flow

let tests = [
  // delegatesFocus readOnly
  function(root: ShadowRoot) {
    // fails
    root.delegatesFocus = true;
  },

  // host readOnly
  function(root: ShadowRoot, element: Element) {
    // fails
    root.host = element;
  },

  // innerHTML
  function(root: ShadowRoot) {
    root.innerHTML = 'test';

    // fails
    root.innerHTML = true;
  },

  // mode readOnly
  function(root: ShadowRoot) {
    // fails
    root.mode = 'open';
  },
];
