import * as React from 'react';
declare const Poly: component<T: React.Node>(children: T) renders T;

declare component Button();
declare component NotAButton();

const el = <Poly><Button /></Poly>;
el as renders NotAButton; // ERROR
el as renders Button; // OK
