//@flow

class C<T> {}

let x = new C(); // Error
let y: C<number> = new C(); // No error
