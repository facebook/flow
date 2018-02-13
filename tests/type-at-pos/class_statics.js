// @flow

class A<T> {
  static f: T
  static mm(x: T) {
    this.f;
  }
}

class B<T> extends A<T> {
  static f: T
  static mm(x: T) {
    this.f;
    super.mm(x);
  }
  static nn() {
    this.mm(this.f);  // excercises the "class_t (Ty.This)" case
  }
}
