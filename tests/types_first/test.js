const n: string = require('./import-value1'); // error: number ~/~> string
import { f } from './import-value2';
const o = require('./import-value3');
import { type T } from './import-type1';
import { type S } from './import-type2';
import { type O } from './import-type5';

(n: T); // error: string ~/~> number
(f(""): S); // error: string (argument) ~/~> number, string (return) ~/~> number
(o: O); // error: number (property x) <~/~> string
(o.y: O); // error: number (property x) <~/~> string

import {
  functionWithTypeof,
  functionWithTypeofAndDestructuring,
  functionWithTypeofAndOptional,
} from './function';
import {ComponentWithTypeof} from './component';
import React from 'react';
functionWithTypeof('', 3, {boz: 3}); // error: number ~> string
functionWithTypeofAndDestructuring({x: [], y: ''}, '', 3); // error: number ~> string, string ~> number
functionWithTypeofAndOptional(1, ''); // error: string, string ~> number
functionWithTypeofAndOptional(undefined, ''); // error: string, string ~> number
<ComponentWithTypeof bar="" baz={3} boz={{boz: 3}} />; // no error: baz and boz are any
