//@flow

// any ~> functionlike
class C { }
var C_any: any = C;
declare var proto_bind: Function$Prototype$Bind;
var proto_bind_any: any = proto_bind;
function f() { }
var f_any: any = f;

// functionlike ~> any
declare var a: any;
var C_from_any: Class<C> = a;
var proto_bind_from_any: Function$Prototype$Bind = a;
var f_from_any: () => void = a;

// any ~> objectlike
var c_any: any = new C();
var obj_any: any = {};
var obj_proto_any: any = Object.prototype;

// objectlike ~> any
var c_from_any: C = a;
var c_from_obj: {| |} = a;
var obj_proto_from_any: typeof Object.prototype = a;

// any ~> get/method
declare var receiver: any;
declare var array: Array<number>;
var test1: string = array[receiver.name]; // if result of receiver.getprop is any, this will fail, but no error if it's empty.
var test2: string = array[receiver.name()] // likewise
