import React from 'react';

declare var any: any;

class Foo extends React.Component<{}, void> {yep1: boolean}
class Bar extends React.Component<{}, void> {yep2: boolean}

(any: $NonMaybeType<React.ElementRef<Class<Foo>>>).yep1; // OK
(any: $NonMaybeType<React.ElementRef<Class<Foo>>>).yep2; // Error
(any: $NonMaybeType<React.ElementRef<Class<Foo>>>).nope; // Error
(any: $NonMaybeType<React.ElementRef<Class<Bar>>>).yep1; // Error
(any: $NonMaybeType<React.ElementRef<Class<Bar>>>).yep2; // OK
(any: $NonMaybeType<React.ElementRef<Class<Bar>>>).nope; // Error
