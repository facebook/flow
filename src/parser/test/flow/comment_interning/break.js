function break_with_label() {
  foo: for (;;) {
    /* 1.1 leading on break */
    break /* 1.2 leading on label */ foo /* 1.3 trailing on label */;
    /* 1.4 trailing */
  }
}

function break_without_label() {
  for (;;) {
    /* 2.1 leading */
    break /* 2.2 internal */;
    /* 2.3 trailing */
  }
}
