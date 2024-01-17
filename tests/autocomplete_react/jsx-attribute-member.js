// @flow

import * as React from 'react';

const foo: { bar: string } = { bar: 'bar' };
(<div className={foo.b}></div>)
//                    ^
