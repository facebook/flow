// @flow

import type { MyUnionType } from './exports-type';
//            ^ --pretty

import {foo} from './exports-type';

const f = foo();
//    ^ --pretty --expand-json-output
