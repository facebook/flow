// @flow

const a = require('./a');

(a: () => void);
(a.g: string);
(a.g: number);
(a.other: string);
