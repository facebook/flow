//@flow

function test1(x: React$AbstractComponent<any, any, any>) { // Error not yet supported
  return x;
}

function test2(x: React$AbstractComponent<any>) { // Not enough targs
  return x;
}

function test3(x: React$AbstractComponent<any,any>) { // Not enough targs
  return x;
}

function test4(x: React$AbstractComponent<any,any,any,any>) { // Too many targs
  return x;
}
