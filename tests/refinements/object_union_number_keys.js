{
  declare const x: {0: 'a', 1: number} | {0: 'b', 1: string};

  if (x['0'] === 'a') {
    x[1] as number; // OK
    x[1] as empty; // ERROR
  } else {
    x[1] as string; // OK
  }

  if (x[0] === 'a') {
    x[1] as number; // OK
    x[1] as empty; // ERROR
  } else {
    x[1] as string; // OK
  }

  switch (x['0']) {
    case 'a': break;
    case 'b': break;
    case 'xxx': break; // ERROR
  }

  switch (x[0]) {
    case 'a': break;
    case 'b': break;
    case 'xxx': break; // ERROR
  }
}

{
  declare const x: {0: 'a', 1: number} | {0: null, 1: string};

  if (x['0'] != null) {
    x[1] as number; // OK
    x[1] as empty; // ERROR
  } else {
    x[1] as string; // OK
  }

  if (x[0] != null) {
    x[1] as number; // OK
    x[1] as empty; // ERROR
  } else {
    x[1] as string; // OK
  }
}
