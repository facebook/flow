// @flow

import * as TypeExports from './exports-type';
//                             ^ --pretty
export * from './exports-type';
//            ^ --pretty
export {p} from './rec-export';
//               ^ --pretty
declare export * from './exports-type';
//                    ^ --pretty
