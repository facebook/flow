//@flow
function f(): void {}

var f: number = 3; // This assignment should not trigger a check for the
// unrecorded write of 3 to f, since this is not an assigning write.
