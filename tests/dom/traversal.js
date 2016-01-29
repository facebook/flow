// @flow

let tests = [
  function() {
    let genericNodeIterator = document.createNodeIterator(document.body);
    let node: ?Text = genericNodeIterator.nextNode(); // invalid
  },

  function() {
    let textNodeIterator = document.createNodeIterator(document.body, 4);
    let textNode: ?Text = textNodeIterator.nextNode(); // valid
  },
];
