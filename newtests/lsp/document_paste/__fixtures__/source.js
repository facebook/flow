// @flow

import type {A} from './import_source';
import foo, {type A as B} from './import_source';
import {bar} from './import_source';
import * as NS from './import_source';
import typeof * as NST from './import_source';
import typeof fooType from './import_source';
import typeof {bar as barType} from './import_source';
import React from 'react';
import bad from 'bad';

foo as A;
foo as B;
const _boo: fooType = foo;
NS as NST;

nested(bar);
React.useState(1);
bad;

function nested(b: barType) {
  b as barType;
}
