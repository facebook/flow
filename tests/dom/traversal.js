// @flow

let tests = [
  function() {
    // document.createNodeIterator() should exist
    (document.createNodeIterator(document.body): any);
  },
  function() {
    // document.createTreeWalker() should exist
    (document.createTreeWalker(document.body): any);
  },
];
