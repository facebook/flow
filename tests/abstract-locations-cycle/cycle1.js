// @flow

import {foo} from './cycle2';

export class Foo {}

const z: Foo = foo;
