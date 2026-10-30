class ReadBeforeAssigned {
  value: string;
  constructor() {
    this.value; // error
    this.value = 'hi';
    this.value;
  }
}

class ReadAfterOneBranch {
  value: string;
  constructor(ready: boolean) {
    if (ready) {
      this.value = 'hi';
    }
    this.value; // error
    this.value = 'bye';
  }
}

class ReadInClosureNotChecked {
  value: string;
  constructor() {
    const read = () => this.value;
    this.value = 'hi';
    read();
  }
}

class WriteInClosureDoesNotCount {
  value: string; // error
  constructor() {
    const write = () => {
      this.value = 'hi';
    };
    write();
  }
}

class ReadOfTypeThatAcceptsUndefined {
  value: ?string;
  constructor() {
    this.value;
  }
}

class CompoundAssignment {
  count: number; // error: `+=` does not count as assigning the field
  constructor() {
    this.count += 1; // error
  }
}

class Increment {
  count: number; // error: `++` does not count as assigning the field
  constructor() {
    this.count++; // error
  }
}

// Logical assignments read the field first, like `if (this.a)`.
class LogicalAssignment {
  a: number;
  b: number;
  c: number; // error: `&&=` leaves an unassigned field unassigned
  constructor() {
    this.a ||= 1; // error
    this.b ??= 1; // error
    this.c &&= 1; // error
  }
}

class PrivateReadBeforeAssigned {
  #value: string;
  constructor() {
    this.#value; // error
    this.#value = 'hi';
  }
}

class PrivateReadAfterAssigned {
  #value: string;
  constructor() {
    this.#value = 'hi';
    this.#value;
  }
}

class DestructuringFromThis {
  value: string;
  constructor() {
    const {value} = this; // error
    this.value = value;
  }
}

class OptionalChainRead {
  value: string;
  constructor() {
    this?.value; // error
    this.value = 'hi';
  }
}
