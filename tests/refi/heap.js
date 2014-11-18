var tests =
[
  function() {
    var x : {p:?string} = {p:"xxx"};
    var y : string = x.p;  // not ok
  },

  function() {
    var x : {p:?string} = {p:"xxx"};
    if (x.p != null) {
      var y : string = x.p;  // ok
    }
  },

  function() {
    var x : {p:?string} = {p:"xxx"};
    if (x.p == null) {} else {
      var y : string = x.p;  // ok
    }
  },

  function() {
    var x : {p:?string} = {p:"xxx"};
    if (!(x.p != null)) {} else {
      var y : string = x.p;  // ok
    }
  },

  function() {
    var x : {p:?string} = {p:"xxx"};
    if (x.p != null) {
      alert("");
      var y : string = x.p;  // not ok
    }
  },

  function() {
    var x : {p:?string} = {p:"xxx"};
    if (x.p != null) {}
    var y : string = x.p;  // not ok
  },

  function() {
    var x : {p:?string} = {p:"xxx"};
    if (x.p != null) {
    } else {
      var y : string = x.p;  // not ok
    }
  },

  function() {
    var x : {p:?string} = {p:"xxx"};
    var y : string = x.p != null ? x.p : ""; // ok
  },

  function() {
    var x : {p:?string} = {p:"xxx"};
    var y : string = x.p || ""; // ok
  },

  function() {
    var x : {p:string | string[]} = {p:["xxx"]};
    if (Array.isArray(x.p)) {
      var y : string[] = x.p; // ok
    } else {
      var z : string = x.p; // ok
    }
  },
];
