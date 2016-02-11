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
  // testing polymorphic return types
  function() {
    const i = document.createNodeIterator(document.body, -1, node => NodeFilter.FILTER_ACCEPT);
    const previousNode: Node | null = i.previousNode();
    const nextNode: Node | null = i.nextNode();
  },
  function() {
    const w = document.createTreeWalker(document.body, -1, node => NodeFilter.FILTER_ACCEPT);
    const parentNode: Node | null = w.parentNode();
    const firstChild: Node | null = w.firstChild();
    const lastChild: Node | null = w.lastChild();
    const previousSibling: Node | null = w.previousSibling();
    const nextSibling: Node | null = w.nextSibling();
    const previousNode: Node | null = w.previousNode();
    const nextNode: Node | null = w.nextNode();
  },
  // testing NodeFilterInterface
  function() {
    // valid
    document.createNodeIterator(document.body, -1, node => NodeFilter.FILTER_ACCEPT);
    // valid
    document.createNodeIterator(document.body, -1, {accept: node => NodeFilter.FILTER_ACCEPT});
    // invalid
    document.createNodeIterator(document.body, -1, {});
  },
  function() {
    // valid
    document.createTreeWalker(document.body, -1, node => NodeFilter.FILTER_ACCEPT);
    // valid
    document.createTreeWalker(document.body, -1, {accept: node => NodeFilter.FILTER_ACCEPT});
    // invalid
    document.createTreeWalker(document.body, -1, {});
  },
];
