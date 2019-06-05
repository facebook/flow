// @flow

const a = require('./a');

(a: () => void);
(a.x: number);
(a.x: string);
(a.other: number);
