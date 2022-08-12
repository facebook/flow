// @flow

import R1 from 'react';
import R1 from 'react'; // error
R1 = (42: any); // error

import * as R2 from 'react';
import R2 from 'react'; // error
R2 = (42: any); // error

import type {Node} from 'react';
const Node = 3; // error
Node = 4; // error
type Node = number; // error
