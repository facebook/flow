import type {T as DefaultT} from './default_export';

const Default = require('./default_export');
const Named = require('./named_export');

// Default export
Default as number;
1 as DefaultT;
1 as Default.T; // error T missing in number

// Object of named exports
Named.foo as number;
1 as Named.T;
