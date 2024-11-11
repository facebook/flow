import * as React from 'react';
import Foo from './base';
import typeof Foo1 from './base';

export type C = Foo1<>;

// previously, the instantiation of Foo below collide with the instantiation of Foo1 above
// because both have the same poly_id, even though they evaluate to different types due to
// ValueToTypeTransform for Foo but not for Foo1.
declare export default function expectFooProps(props: React.PropsOf<Foo<>>): void;
