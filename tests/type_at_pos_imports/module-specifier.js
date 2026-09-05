// @flow

import * as TypeExports from './exports-type';
//                             ^
export * from './exports-type';
//            ^
export {p} from './rec-export';
//               ^
declare export * from './exports-type';
//                    ^
