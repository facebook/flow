// @flow

let listener: EventListener = function (event: Event): void {};

let tests = [
  // attachEvent
  function () {
    let target = new EventTarget();
    target.attachEvent('foo', listener) as void; // invalid, may be undefined
    (target.attachEvent && target.attachEvent('foo', listener)) as void; // valid
  },

  // detachEvent
  function () {
    let target = new EventTarget();
    target.detachEvent('foo', listener) as void; // invalid, may be undefined
    (target.detachEvent && target.detachEvent('foo', listener)) as void; // valid
  },

  function () {
    window.onmessage = (event: MessageEvent) => {
      event.target as window;
    };
  },
];
