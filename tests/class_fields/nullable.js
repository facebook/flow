class NullableCheck {

  obj:?{ call: Function, apply: Function };

  constructor(obj:?{ call: Function, apply: Function }) {
    this.obj = obj;
  }

  apply() {
    if(this.obj) {
      this.obj.call();
      this.obj.apply();
    }
  }
}
