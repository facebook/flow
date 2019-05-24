/* @flow */

(process.allowedNodeEnvironmentFlags: Set<string>);

(process.allowedNodeEnvironmentFlags: string); // error

process.once('unhandledRejection', e => {
  return e + 1; // error
});

process.once('unhandledRejection', e => {
  return e + 1; // error
});

process.on('unhandledRejection', e => {
  (process.allowedNodeEnvironmentFlags: mixed);
});

process.once('unhandledRejection', e => {
  (process.allowedNodeEnvironmentFlags: mixed);
});
