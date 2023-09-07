import * as React from 'react';

declare const x: React.RefSetter<number | string>;
(x: React.RefSetter<number>); // OK!
(x: React$RefSetter<number>); // OK!

declare const y: React$RefSetter<number | string>;
(y: React.RefSetter<number>); // OK!
(y: React$RefSetter<number>); // OK!

(x: React.RefSetter<number | string | boolean>); // ERROR
(y: React.RefSetter<number | string | boolean>); // ERROR
