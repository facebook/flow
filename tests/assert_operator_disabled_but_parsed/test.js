function B() {
  let a = 10;
  a!
  a! = 42;
  a! += 42;
  let b = { x: 20 }
  b.x!
  b.x! = 42;
  b.x! += 42;
  let c = { x: {y : 20} }
  c?.x!.y;
  c?.x!['y'];
  delete c!.x;
  let d = { x: { y: () => 20 } }
  d?.x!.y!();
}
