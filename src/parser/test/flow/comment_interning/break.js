function break_with_label() {
  foo: for (var i = 0; i < 10; i++) {
    /* 1.1 leading on break */
    break /* 1.2 leading on label */ foo /* 1.3 trailing */;
    /* 1.4 trailing */
  }
}

function break_without_label() {
  for (var i = 0; i < 10; i++) {
    /* 2.1 leading */
    break;
    /* 2.2 trailing */
  }
}
