function forLoop(): string {
  let i = "hi";

  for (let i = 0; i < 10; i++) {
  }

  return i;
}

/* TODO: function TDZ1() {
  let foo;
  let foo; // OK
  if (true) {
    let bar;
    let bar; // TypeError
  }
} */

/* TODO: function TDZ2(x) {
  switch (x) {
    case 0:
      let foo;
      break;

    case 1:
      let foo; // TypeError
      break;
  }
} */
