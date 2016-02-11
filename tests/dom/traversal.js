// @flow

let tests = [
  // basic functionality
  function() {
    const i: NodeIterator = document.createNodeIterator(document.body);
    const filter: NodeFilter = i.filter;
    const response: typeof NodeFilter.FILTER_ACCEPT | typeof NodeFilter.FILTER_REJECT | typeof NodeFilter.FILTER_SKIP = filter.acceptNode(document.body);
  },
  function() {
    const w: TreeWalker = document.createTreeWalker(document.body);
    const filter: NodeFilter = w.filter;
    const response: typeof NodeFilter.FILTER_ACCEPT | typeof NodeFilter.FILTER_REJECT | typeof NodeFilter.FILTER_SKIP = filter.acceptNode(document.body);
  },
  // testing NodeFilterInterface
  function() {
    // valid
    document.createNodeIterator(document.body, undefined, node => NodeFilter.FILTER_ACCEPT);
    // valid
    document.createNodeIterator(document.body, undefined, {accept: node => NodeFilter.FILTER_ACCEPT});
    // invalid
    document.createNodeIterator(document.body, undefined, {});
  },
  function() {
    // valid
    document.createTreeWalker(document.body, undefined, node => NodeFilter.FILTER_ACCEPT);
    // valid
    document.createTreeWalker(document.body, undefined, {accept: node => NodeFilter.FILTER_ACCEPT});
    // invalid
    document.createTreeWalker(document.body, undefined, {});
  },
];
