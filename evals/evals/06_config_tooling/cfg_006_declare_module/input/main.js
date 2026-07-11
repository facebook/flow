// @flow

import {chunk} from 'list-utils';

export const pages: Array<Array<number>> = chunk([1, 2, 3, 4, 5], 2);
