declare const x: 0;

match (x) {
  0: {
    while (true) {
      break; // OK
    }
    switch (x) {
      case 0:
        break; // OK
    }
  }
}

match (x) {
  0: {
    break; // ERROR: parse error
  }
}
