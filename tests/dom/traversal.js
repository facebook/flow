// @flow

let tests = [
  function() {
    // document.createNodeIterator() should exist
    (document.createNodeIterator(document.body): NodeIterator);
  },
  function() {
    // document.createTreeWalker() should exist
    (document.createTreeWalker(document.body): TreeWalker);
  },
];
