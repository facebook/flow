//@flow

import {x as y, type T} from './export';
import X from './export';
import * as Y from './export';

var w: T = 'a'; // err
y as empty; // err
X as empty; // err
Y.x as empty; // err
