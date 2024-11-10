import * as React from 'react';
import Foo from './base';
import typeof Foo1 from './base';

export type C = Foo1<>;

declare export default function expectFooProps(props: React.PropsOf<Foo<>>): void; // busted due to subst cache collision
