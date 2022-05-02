// @flow

function if_tests() {
  function merge_with_general_annotated() {
    const foo: Array<string> = [];
    let bar: Array<any> = foo;
    if (true) bar = foo;
    (bar: Array<boolean>); // ok
  }

  function merge_with_general_undefined() {
    let bar: Array<boolean>;
    if (true) bar = [];
    (bar: Array<boolean>); // error
  }
}

function switch_tests() {
  function merge_with_general_annotated() {
    const foo: Array<string> = [];
    let bar: Array<any> = foo;
    switch (1) {
      case 1:
        bar = foo;
    }
    (bar: Array<boolean>); // ok
  }

  function merge_with_general_undefined() {
    let bar: Array<boolean>;
    switch (1) {
      case 1:
        bar = [];
    }
    (bar: Array<boolean>); // error
  }
}

function try_catch_tests() {
  function merge_with_general_annotated() {
    const foo: Array<string> = [];
    let bar: Array<any> = foo;
    try {
      ;
      bar = foo;
    } catch {}
    (bar: Array<boolean>); // ok
  }

  function merge_with_general_undefined() {
    let bar: Array<boolean>;
    try {
      ;
      bar = [];
    } catch {}
    (bar: Array<boolean>); // error
  }
}


function loop_tests() {
  function merge_with_general_annotated() {
    const foo: Array<string> = [];
    let bar: Array<any> = foo;
    while (true) {
      bar = foo;
    }
    (bar: Array<boolean>); // ok
  }

  function merge_with_general_undefined() {
    let bar: Array<boolean>;
    while (true) {
      bar = [];
    }
    (bar: Array<boolean>); // error
  }
}
