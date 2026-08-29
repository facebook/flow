// @flow

import {ImportedClass} from './lib';

declare const imported: ImportedClass;
const method = imported.method; // OK
method(); // ERROR: a standalone call does not provide the required receiver
