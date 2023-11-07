// @flow

type S = ?{type: 'foo'};
declare var x: S;

if (x?.type === 'foo') {
  x.type as 'foo';
  x.type as 'foo';
  x as {type: 'foo'};
} else {
  x as null | void;
}

if (x?.type !== 'foo') {
  x as null | void;
} else {
  x.type as 'foo';
  x.type as 'foo';
  x as {type: 'foo'};
}
