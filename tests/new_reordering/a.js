//@flow


class TimeOfDay {
  a(other: TimeOfDay): boolean {
    return true
  }
}

class TimeOfDay_noanno {
  a(other: TimeOfDay_noanno): boolean {
    return true
  }
}

class TimeOfDay_noanno2 {
  a(): void {
    return this.a();
  }
}

function f(): number {
  return f();
}

function f_na() {
  return f_na();
}

function havoc_x() {
  x = null;
}

var x;
havoc_x();
x = x;

function a() {
  return b()
}

function b() {
  return a()
}
