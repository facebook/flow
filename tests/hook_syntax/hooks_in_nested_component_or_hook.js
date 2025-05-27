import React, {useState} from 'react';

component Foo() {
  hook useNested() { // error: nested-hook
    return useState(); // error: react-rule-hook error
  }

  useNested();

  if (false) {
    return;
  }
  component Bar() { // error: nested-component
    useState(); // ok: no react-rule-hook-conditional error
    if (true) {
      useState(); // error: react-rule-hook-conditional
    }
    return;
  }

  useNested(); // error: react-rule-hook-conditional

  return <Bar />;
}

hook useFoo() {
  hook useNested() { // error: nested-hook
    return useState(); // error: react-rule-hook error
  }

  useNested();

  if (false) {
    return;
  }
  component Bar() { // error: nested-component
    useState(); // ok: no react-rule-hook-conditional error
    if (true) {
      useState(); // error: react-rule-hook-conditional
    }
    return;
  }

  useNested(); // error: react-rule-hook-conditional

  return <Bar />;
}
