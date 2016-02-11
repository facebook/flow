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
  // WhatToShowT
  function() {
    const i = document.createNodeIterator(document.body, NodeFilter.SHOW_ELEMENT);
    const previousNode: Element | null = i.previousNode();
    const nextNode: Element | null = i.nextNode();
  },
  function() {
    const i = document.createNodeIterator(document.body, NodeFilter.SHOW_ATTRIBUTE);
    const previousNode: Attr | null = i.previousNode();
    const nextNode: Attr | null = i.nextNode();
  },
  function() {
    const i = document.createNodeIterator(document.body, NodeFilter.SHOW_TEXT);
    const previousNode: Text | null = i.previousNode();
    const nextNode: Text | null = i.nextNode();
  },
  function() {
    const i = document.createNodeIterator(document.body, NodeFilter.SHOW_DOCUMENT);
    const previousNode: Document | null = i.previousNode();
    const nextNode: Document | null = i.nextNode();
  },
  function() {
    const i = document.createNodeIterator(document.body, NodeFilter.SHOW_DOCUMENT_TYPE);
    const previousNode: DocumentType | null = i.previousNode();
    const nextNode: DocumentType | null = i.nextNode();
  },
  function() {
    const i = document.createNodeIterator(document.body, NodeFilter.SHOW_DOCUMENT_FRAGMENT);
    const previousNode: DocumentFragment | null = i.previousNode();
    const nextNode: DocumentFragment | null = i.nextNode();
  },
  function() {
    const i = document.createNodeIterator(document.body, NodeFilter.SHOW_ALL);
    const previousNode: Node | null = i.previousNode();
    const nextNode: Node | null = i.nextNode();
  },
  function() {
    const w = document.createTreeWalker(document.body, NodeFilter.SHOW_ELEMENT);
    const parentNode: Element | null = w.parentNode();
    const firstChild: Element | null = w.firstChild();
    const lastChild: Element | null = w.lastChild();
    const previousSibling: Element | null = w.previousSibling();
    const nextSibling: Element | null = w.nextSibling();
    const previousNode: Element | null = w.previousNode();
    const nextNode: Element | null = w.nextNode();
  },
  function() {
    const w = document.createTreeWalker(document.body, NodeFilter.SHOW_ATTRIBUTE);
    const parentNode: Attr | null = w.parentNode();
    const firstChild: Attr | null = w.firstChild();
    const lastChild: Attr | null = w.lastChild();
    const previousSibling: Attr | null = w.previousSibling();
    const nextSibling: Attr | null = w.nextSibling();
    const previousNode: Attr | null = w.previousNode();
    const nextNode: Attr | null = w.nextNode();
  },
  function() {
    const w = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
    const parentNode: Text | null = w.parentNode();
    const firstChild: Text | null = w.firstChild();
    const lastChild: Text | null = w.lastChild();
    const previousSibling: Text | null = w.previousSibling();
    const nextSibling: Text | null = w.nextSibling();
    const previousNode: Text | null = w.previousNode();
    const nextNode: Text | null = w.nextNode();
  },
  function() {
    const w = document.createTreeWalker(document.body, NodeFilter.SHOW_DOCUMENT);
    const parentNode: Document | null = w.parentNode();
    const firstChild: Document | null = w.firstChild();
    const lastChild: Document | null = w.lastChild();
    const previousSibling: Document | null = w.previousSibling();
    const nextSibling: Document | null = w.nextSibling();
    const previousNode: Document | null = w.previousNode();
    const nextNode: Document | null = w.nextNode();
  },
  function() {
    const w = document.createTreeWalker(document.body, NodeFilter.SHOW_DOCUMENT_TYPE);
    const parentNode: DocumentType | null = w.parentNode();
    const firstChild: DocumentType | null = w.firstChild();
    const lastChild: DocumentType | null = w.lastChild();
    const previousSibling: DocumentType | null = w.previousSibling();
    const nextSibling: DocumentType | null = w.nextSibling();
    const previousNode: DocumentType | null = w.previousNode();
    const nextNode: DocumentType | null = w.nextNode();
  },
  function() {
    const w = document.createTreeWalker(document.body, NodeFilter.SHOW_DOCUMENT_FRAGMENT);
    const parentNode: DocumentFragment | null = w.parentNode();
    const firstChild: DocumentFragment | null = w.firstChild();
    const lastChild: DocumentFragment | null = w.lastChild();
    const previousSibling: DocumentFragment | null = w.previousSibling();
    const nextSibling: DocumentFragment | null = w.nextSibling();
    const previousNode: DocumentFragment | null = w.previousNode();
    const nextNode: DocumentFragment | null = w.nextNode();
  },
  function() {
    const w = document.createTreeWalker(document.body, NodeFilter.SHOW_ALL);
    const parentNode: Node | null = w.parentNode();
    const firstChild: Node | null = w.firstChild();
    const lastChild: Node | null = w.lastChild();
    const previousSibling: Node | null = w.previousSibling();
    const nextSibling: Node | null = w.nextSibling();
    const previousNode: Node | null = w.previousNode();
    const nextNode: Node | null = w.nextNode();
  },
  // NodeFilterInterface
  function() {
    document.createNodeIterator(document.body, -1, node => NodeFilter.FILTER_ACCEPT); // valid
    document.createNodeIterator(document.body, -1, {accept: node => NodeFilter.FILTER_ACCEPT}); // valid
    document.createNodeIterator(document.body, -1, {}); // invalid
  },
  function() {
    document.createTreeWalker(document.body, -1, node => NodeFilter.FILTER_ACCEPT); // valid
    document.createTreeWalker(document.body, -1, {accept: node => NodeFilter.FILTER_ACCEPT}); // valid
    document.createTreeWalker(document.body, -1, {}); // invalid
  },
];
