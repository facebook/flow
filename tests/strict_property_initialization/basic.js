class AssignedInConstructor {
  value: string;
  constructor() {
    this.value = 'hi';
  }
}

class NeverAssigned {
  value: string; // error
}

class AssignedOnOneBranch {
  value: string; // error
  constructor(ready: boolean) {
    if (ready) {
      this.value = 'hi';
    }
  }
}

class AssignedOnBothBranches {
  value: string;
  constructor(ready: boolean) {
    if (ready) {
      this.value = 'hi';
    } else {
      this.value = 'bye';
    }
  }
}

class EarlyReturn {
  value: string; // error
  constructor(ready: boolean) {
    if (!ready) {
      return;
    }
    this.value = 'hi';
  }
}

class ThrowIsNotAnExit {
  value: string;
  constructor(ready: boolean) {
    if (!ready) {
      throw 'not ready';
    }
    this.value = 'hi';
  }
}

class Initialized {
  value: string = 'hi';
}

class Declared {
  declare value: string;
}

class PrivateFields {
  #value: string; // error
  #other: string;
  constructor() {
    this.#other = 'hi';
  }
}

class SubclassWithoutConstructor extends AssignedInConstructor {
  extra: number; // error
}

class Subclass extends AssignedInConstructor {
  extra: number;
  constructor() {
    super();
    this.extra = 1;
  }
}
