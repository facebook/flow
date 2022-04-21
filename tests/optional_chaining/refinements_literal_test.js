// @flow

type S = ?{type: 'foo'};
declare var x: S;

if (x?.type === 'foo') {
  (x.type: 'foo');
  (x.type: 'foo');
  (x: {type: 'foo'});
} else {
  (x: null | void)
}

if (x?.type !== 'foo') {
  (x: null | void)
} else {
  (x.type: 'foo');
  (x.type: 'foo');
  (x: {type: 'foo'});
}
