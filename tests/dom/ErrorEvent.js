// @flow

let tests = [
  // ErrorEvent
  function() {
    const event = new ErrorEvent('error', {
      message: 'An error occurred, and what an event!',
      lineno: 1000,
    });
    (event.message: string);
    (event.filename: string);
    (event.lineno: number);
    (event.colno: number);
    (event.error: Object | null);
  }
];
