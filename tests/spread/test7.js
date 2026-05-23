let tests = [
  function(x: any) {
    ({...x}) as interface {};
    ({...x}) as void;
  },
];
