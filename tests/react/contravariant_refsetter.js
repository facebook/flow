import * as React from 'react';

declare const x: React.RefSetter<number | string>;
x as React.RefSetter<number>; // OK!

declare const y: React.RefSetter<number | string>;
y as React.RefSetter<number>; // OK!

x as React.RefSetter<number | string | boolean>; // ERROR
y as React.RefSetter<number | string | boolean>; // ERROR
