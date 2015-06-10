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

function forOfLoop(): number {
  let x = 1;

  for (let x of "abc") {}

  return x;
}

function TDZ1() {
  let foo;
  let foo; // error
  if (true) {
    let bar;
    let bar; // error
  }
}

function TDZ2(x) {
  switch (x) {
    case 0:
      let foo;
      break;

    case 1:
      let foo; // error
      break;
  }
}
