var a: any = null;

var obj1 : { x: number } & { y: string} = a;
// TODO: the following shouldn't be an error
var obj2 : { x: number; y: string } = obj1;

var fun1: ((_: number) => void) & ((_: string) => void) = a;
// TODO: the following shouldn't be an error
var fun2 : (_: number | string) => void = obj1;
