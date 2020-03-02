function onlyLeading() {
  /* Not leading 1 */
  const z = 0;
  // 1.1 Leading A
  /* 1.2 Leading B */
  debugger;
}

function onlyTrailing() {
  debugger /* 2.1 trailing */ ;
  /* 2.2 Not Trailing */
}

function leadingAndTrailing() {
  // 3.1 Leading A
  /* 3.2 Leading B */
  debugger /* 3.3 trailing */ ;
  // 3.4 Not Trailing
}
