import React from 'react';

type Comp<Props: {...}> = component(ref: React.RefSetter<string>, ...Props);

function test1() {
  declare const Base: Comp<{}>;
  declare component C(ref: React.RefSetter<React.ElementRef<typeof Base>>);

  declare const ref: React.RefSetter<React.ElementRef<typeof C>> | void;
  <C ref={ref} />; // ok
}
