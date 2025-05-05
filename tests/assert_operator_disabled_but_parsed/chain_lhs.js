function B() {
    let c = { x: {y : 20} }
    c?.x!.y = 42;
    c?.x!['y'] = 42;
  }
