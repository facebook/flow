/* @flow */

type Shape =
  {type: 'rectangle', width: number, height: number} |
  {type: 'circle', radius: number};

function area(shape: Shape): number {
  if (shape.type === 'square') { // error
    return shape.width * shape.height;
  } else if (shape.type === 'circle') { // ok
    return Math.PI * Math.pow(shape.radius, 2);
  }
  throw "unreachable"; // TODO: this shouldn't be needed
}

type ExactShape =
  {|type: 'rectangle', width: number, height: number|} |
  {|type: 'circle', radius: number|};

function area2(shape: ExactShape): number {
  if (shape.type === 'square') { // error
    return shape.width * shape.height;
  } else if (shape.type === 'circle') { // ok
    return Math.PI * Math.pow(shape.radius, 2);
  }
  throw "unreachable"; // TODO: this shouldn't be needed
}

type ReadOnlyShape =
  {+type: 'rectangle', width: number, height: number} |
  {+type: 'circle', radius: number};

function area3(shape: ReadOnlyShape): number {
  if (shape.type === 'square') { // error
    return shape.width * shape.height;
  } else if (shape.type === 'circle') { // ok
    return Math.PI * Math.pow(shape.radius, 2);
  }
  throw "unreachable"; // TODO: this shouldn't be needed
}

function singleton() {
  const A: 'A' = 'A';
  const B: 'B' = 'B';
  const C: 'C' = 'C';
  const D: 'D' = 'D';
  const OBJ = Object.freeze({A, B, C, D});

  type AorB = {type: 'A'} | {type: 'B'};
  type AorC = {type: 'A'} | {type: 'C'};

  function test1(x: AorB) {
    switch (x.type) {
      case A: break;
      case B: break;
      case C: break; // error C is not included in 'A' | 'B'
      case D: break; // error D is not included in 'A' | 'B'
    }
  }

  function test1a(x: AorB) {
    switch (x.type) {
      case OBJ.A: break;
      case OBJ.B: break;
      case OBJ.C: break; // error C is not included in 'A' | 'B'
      case OBJ.D: break; // error D is not included in 'A' | 'B'
    }
  }

  function test2(x: AorB) {
    if (x.type === A) {};
    if (x.type === B) {};
    if (x.type === C) {}; // error C is not included in 'A' | 'B'
    if (x.type === D) {}; // error D is not included in 'A' | 'B'
  }

  function test3(x: AorB & AorC) {
    switch (x.type) {
      case A: break;
      case B: break; // error B is not included in 'A'
      case C: break; // error C is not included in 'A'
      case D: break; // error D is not included in 'A'
    }
  }

  function test4(x: AorB & AorC) {
    if (x.type === A) {};
    if (x.type === B) {}; // error B is not included in 'A'
    if (x.type === C) {}; // error C is not included in 'A'
    if (x.type === D) {}; // error D is not included in 'A'
  }

  type AandP = {type: 'A', ...} & {prop: 1, ...};
  type BandP = {type: 'B', ...} & {prop: 1, ...};

  function test5(x: AandP | BandP) {
    switch (x.type) {
      case A: break;
      case B: break;
      case C: break; // error C is not included in 'A'|'B'
      case D: break; // error D is not included in 'A'|'B'
    }
  }

  function test6(x: AandP | BandP) {
    if (x.type === A) {};
    if (x.type === B) {};
    if (x.type === C) {}; // error C is not included in 'A'|'B'
    if (x.type === D) {}; // error D is not included in 'A'|'B'
  }
}

function literal_types() {
  const A = 'A';
  const B = 'B';
  const C = 'C';
  const D = 'D';

  type AorB = {type: 'A'} | {type: 'B'};
  type AorC = {type: 'A'} | {type: 'C'};

  function test1(x: AorB) {
    switch (x.type) {
      case A: break;
      case B: break;
      case C: break; // error C is not included in 'A' | 'B'
      case D: break; // error D is not included in 'A' | 'B'
    }
  }

  function test2(x: AorB) {
    if (x.type === A) {};
    if (x.type === B) {};
    if (x.type === C) {}; // error C is not included in 'A' | 'B'
    if (x.type === D) {}; // error D is not included in 'A' | 'B'
  }

  function test3(x: AorB & AorC) {
    switch (x.type) {
      case A: break;
      case B: break; // error B is not included in 'A'
      case C: break; // error C is not included in 'A'
      case D: break; // error D is not included in 'A'
    }
  }

  function test4(x: AorB & AorC) {
    if (x.type === A) {};
    if (x.type === B) {}; // error B is not included in 'A'
    if (x.type === C) {}; // error C is not included in 'A'
    if (x.type === D) {}; // error D is not included in 'A'
  }

  type AandP = {type: 'A', ...} & {prop: 1, ...};
  type BandP = {type: 'B', ...} & {prop: 1, ...};

  function test5(x: AandP | BandP) {
    switch (x.type) {
      case A: break;
      case B: break;
      case C: break; // error C is not included in 'A'|'B'
      case D: break; // error D is not included in 'A'|'B'
    }
  }

  function test6(x: AandP | BandP) {
    if (x.type === A) {};
    if (x.type === B) {};
    if (x.type === C) {}; // error C is not included in 'A'|'B'
    if (x.type === D) {}; // error D is not included in 'A'|'B'
  }
}
