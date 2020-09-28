// @flow

import type {TypeExport} from './foo';
import {type TypeExport as TypeExportRenamed} from './foo';
import typeof {ClassExport} from './foo';
import {typeof ClassExport as ClassExportRenamed} from './foo';

// Does not error, since only types were imported
const Foo = require('./foo');
