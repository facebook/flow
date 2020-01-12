function leading() {
  /* 1.0 unreachable leading */
  var F = function(){}; /* 1.1 leading */
  /* 1.2 leading */
  new F();
}

function trailing() {
  var F = function(){};

  new F() /* 2.0 trailing */;
  /* 2.1 unreachable trailing */
}

function leading_and_trailing() {
  /* 3.0 unreachable leading */
  var F = function(){}; /* 3.1 leading */
  /* 3.2 leading */
  new /* 3.3 unreachable by New node */ F() /* 3.4 trailing */;
  /* 3.5 unreachable trailing */
}
