// @flow

function create(Base) {
  return class extends Base {
    constructor() {
      super();
    }
  };
}

class A {}
class B {}
create(A);
create(B);
