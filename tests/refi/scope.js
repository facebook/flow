// refinements should not escape variable scope

var tests =
[
  function() {
    {
      let x : {p:?string} = {p:"xxx"};
      if (x.p == null) {
        return;
      }
    }
    x.p; // not ok
  },

  function() {
    var x : {p:?string} = {p:"xxx"};
    {
      if (x.p == null) {
        return;
      }
    }
    var y: string = x.p; // ok
  },

  function() {
    var x : {p:?string} = {p:"xxx"};

    function inner() {
      if (x.p == null) {
        return;
      }
    }

    var y: string = x.p; // not ok
  }
]
