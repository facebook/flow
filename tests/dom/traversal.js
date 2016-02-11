// @flow

let tests = [
  function() {
    const i: NodeIterator = document.createNodeIterator(document.body);
    const filter: NodeFilter = i.filter;
  },
  function() {
    const w: TreeWalker = document.createTreeWalker(document.body);
    const filter: NodeFilter = w.filter;
  },
];
