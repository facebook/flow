function forLoop(): string {
  let i = "hi";

  for (let i = 0; i < 10; i++) {
  }

  return i;
}

function forInLoop(): number {
  let prop = 1;
  let obj = { foo: true };

  for (let prop in obj) {
    let val: boolean = obj[prop];
  }

  return prop;
}

function TDZ1() {
  let foo;
  let foo; // OK
  if (true) {
    let bar;
    let bar; // TypeError
  }
}

function TDZ2(x) {
  switch (x) {
    case 0:
      let foo;
      break;

    case 1:
      let foo; // TypeError
      break;
  }
}
