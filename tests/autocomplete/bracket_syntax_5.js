//@flow

declare var obj : { foo: number, bar : number }

// This one is a TODO; test is to document a known problem
// There isn't currently a good way for us to detect that we should treat this
// as member autocomplete
let x = obj[ + obj["foo"]
//          ^
