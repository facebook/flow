// @flow

import {ImportedClass} from './lib';

declare const imported: ImportedClass;
const method = imported.method; // ERROR: the consumer is .js
method();
