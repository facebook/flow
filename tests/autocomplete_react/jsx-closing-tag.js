// @flow

import * as React from 'react';

type Props = $ReadOnly<{x: number, y: string}>;

function Foo(props: Props) {}
function Bar(props: Props) {}

(<Foo><Bar></></Foo>)
//           ^
