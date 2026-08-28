// @flow

import {ImportedClass} from './lib';

declare const imported: ImportedClass;
const method = imported.method; // OK
method(); // OK
