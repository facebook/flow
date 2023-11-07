// @flow

let tests = [
  // ErrorEvent
  function () {
    const event = new ErrorEvent('error', {
      message: 'An error occurred, and what an event!',
      lineno: 1000,
    });
    event.message as string;
    event.filename as string;
    event.lineno as number;
    event.colno as number;
    event.error as any;
  },
];
