// @flow

let tests = [
  // createElement
  function(document: Document) {
    (document.createElement('canvas'): HTMLCanvasElement);
    (document.createElement('option'): HTMLOptionElement);
    (document.createElement('select'): HTMLSelectElement);
  }
];
