// @flow

let tests = [
  // createElement
  function (document: Document) {
    document.createElement('canvas') as HTMLCanvasElement;
    document.createElement('link') as HTMLLinkElement;
    document.createElement('option') as HTMLOptionElement;
    document.createElement('select') as HTMLSelectElement;
    document.querySelector('select') as HTMLSelectElement | null;
    document.createElement('hr') as HTMLElement; // GH #3752
  },
  function (document: Document) {
    document.head as HTMLHeadElement | null;
  },
];
