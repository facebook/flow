let tests = [
  function(x: Object) {
    ({...x}) as Object;
    ({...x}) as void; // error, Object
  },
];
