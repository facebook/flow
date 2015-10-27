var x = Boolean(4);
function foo(fn:(value:any)=>boolean) { }
foo(Boolean);

var dict: { [k: string]: any } = {};
dict(); // error, callable signature not found
