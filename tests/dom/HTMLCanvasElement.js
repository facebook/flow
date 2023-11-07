// @flow

let tests = [
  // getContext
  function (el: HTMLCanvasElement) {
    el.getContext('2d') as ?CanvasRenderingContext2D;
  },
];
