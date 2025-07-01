{
    declare var x: boolean;
    if (x) {
        if (x) {} // error, x is refined to `true`
    }

    if (!x) {
        if (x) {} // error, x is refined to `false`
    }
}

{
    function intersection(
      arrays: $ReadOnlyArray<$ReadOnlyArray<number>>,
    ) {
      if (!arrays.length) {
        return;
      }

      let a: number = arrays.length;
      let b = a;

      if(a) {} // not inferred to be truthy
      if(b) {} // inferred to be truthy

      // let c = arrays.length;
      // b = arrays.length; // uncomment this line will make `b` above to be not truthy. amusingly odd.
    }
  }
