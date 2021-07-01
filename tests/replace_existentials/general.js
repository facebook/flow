//@flow
const x: * = 1;

const arr: Array<*> = [];
arr.push('test');

const arr2: Array<*> = [1, 'test'];

class TestClass {
  x: *;
  constructor() {
    this.x = 1;
  }
}

// $FlowFixMe[cannot-resolve-name]
function search(entry: SearchableEntry<*>): void{
}

