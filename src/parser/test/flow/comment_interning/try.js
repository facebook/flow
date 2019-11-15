function onlyLeading() {
  /* Not leading */
  const z = 0;
  // 1.1 Leading A
  /* 1.2 Leading B */ try {
    const x = 1;
  } catch (e1) {
    console.log(1);
  }
}

function onlyTrailing() {
  try /* 2.1 Trailing */ {
    /* 2.2 Not trailing */
    const y = 2;
  } catch (e2) {
    console.log(2);
  }
  /* 2.3 Not try Trailing B */
}

function leadingAndTrailing() {
  // 3.1 Leading
  try /* 3.2 Trailing */ {
    /* 3.3 Not trailing */
    const y = 2;
  } finally {
    console.log(3);
  }
  /* 3.4 Not Trailing */
}
