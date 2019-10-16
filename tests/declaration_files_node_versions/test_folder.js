//@flow

import {version} from 'hello';
import data from 'hello/foo';

;(version: '>=0.98.0') // no errors
;(data: number) // no errors
