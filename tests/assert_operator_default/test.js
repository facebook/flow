// Flow parses assertions by default but rejects their use without opt-in.
function assertions(value: ?{prop: ?number, method: ?(() => number), child: ?{prop: number}}) {
  value!; // ERROR
  value?.prop!; // ERROR
  value?.child!.prop; // ERROR
  value?.prop! < 1; // ERROR
  value?.prop!! < 1; // ERROR
  value?.method!(); // ERROR
  value!.prop! = 1; // ERROR
  value!.prop! += 1; // ERROR
  delete value!.prop; // ERROR
}

// ASI leaves next-line prefix negation outside the assertion type gate.
function negation(value: number, other: number) {
  value
  !other; // OK
  value /*
  */ !other; // OK
}
