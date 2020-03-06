function withExpression() {
  /* 1.1 Leading return */ return /* 1.2 Leading num */ 1 /* 1.3 Trailing num */; /* 1.4 Trailing return */
}

function withoutExpression() {
  /* 2.1 Leading return */ return /* 2.2 inner, ignored */ ; /* 2.3 Trailing return */
}
