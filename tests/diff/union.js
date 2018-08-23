declare var any: any;

class A {}
class B {}
class C {}

((any: {a: A} | {b: B}): $Diff<{a: A} | {b: B}, {b: B}>); // OK
((any: {a: A} | {}): $Diff<{a: A} | {b: B}, {b: B}>); // OK
((any: {} | {b: B}): $Diff<{a: A} | {b: B}, {b: B}>); // OK
((any: {a: A, c: C}): $Diff<{a: A, b: B, c: C}, {a: A} | {b: B}>); // OK
((any: {b: B, c: C}): $Diff<{a: A, b: B, c: C}, {a: A} | {b: B}>); // OK
((any: {} | {b: B}): $Diff<{a: A} | {b: B}, {a: A}>); // OK
