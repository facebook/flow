let tests = [
  function(x: any) {
    ({...x}) as Object;
    ({...x}) as void;
  },
];
