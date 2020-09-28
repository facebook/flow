// @flow

import type {T as DefaultT} from './default_export';

const Default = require('./default_export');
const Named = require('./named_export');

// Default export
(Default : number);
(1 : DefaultT);
(1 : Default.T); // error T missing in number

// Object of named exports
(Named.foo : number);
(1 : Named.T);
