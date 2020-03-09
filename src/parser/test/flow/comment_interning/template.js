function leadingOnly() {
  /* 1.1 Leading */ `template`;
}

function trailingOnly() {
  `template` /* 2.1 Trailing */;
}

function leadingAndTrailing() {
  /* 3.1 Leading */ `template` /* 3.2 Trailing */;
}

function withExpressions() {
  /* 4.1 Leading template */ `test ${/* 4.2 Leading num */ 1 /* 4.3 Trailing num */} test` /* 4.4 Trailing template */;
}
