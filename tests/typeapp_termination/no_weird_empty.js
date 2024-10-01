import React from 'react';

function test1() {
  declare const Base: React.AbstractComponent<{}, string>;
  declare component C(ref: React.RefSetter<React.ElementRef<typeof Base>>);

  declare const ref: React.RefSetter<React.ElementRef<typeof C>> | void;
  <C ref={ref} />; // ok
}
