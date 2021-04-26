// @flow

/**
 * exports a class, which is both a name and a type, and also as a default
 * export, and also the same name as the file (thus the namespace import).
 *
 * import {AllTheThings} from './AllTheThings';
 * import AllTheThings from './AllTheThings';
 * import type AllTheThings from './AllTheThings';
 * import * as AllTheThings from './AllTheThings';
 */

export class AllTheThings {}

export default AllTheThings;
