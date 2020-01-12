function continue_with_label() {
  foo: for (;;) {
    /* 1.1 leading on continue */
    continue /* 1.2 leading on label */ foo /* 1.3 trailing */;
    /* 1.4 trailing */
  }
}

function continue_without_label() {
  for (;;) {
    /* 2.1 leading */
    continue /* 2.2 internal */;
    /* 2.3 trailing */
  }
}
