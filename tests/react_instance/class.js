import React from 'react';

declare var any: any;

class Foo extends React.Component<{}, void> {yep1: boolean}
class Bar extends React.Component<{}, void> {yep2: boolean}

(any as NonNullable<React.ElementRef<Class<Foo>>>).yep1; // OK
(any as NonNullable<React.ElementRef<Class<Foo>>>).yep2; // Error
(any as NonNullable<React.ElementRef<Class<Foo>>>).nope; // Error
(any as NonNullable<React.ElementRef<Class<Bar>>>).yep1; // Error
(any as NonNullable<React.ElementRef<Class<Bar>>>).yep2; // OK
(any as NonNullable<React.ElementRef<Class<Bar>>>).nope; // Error
