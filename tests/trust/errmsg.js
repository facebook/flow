//@flow

var a: $Trusted<number> = 42 as any;
var b: any = 42 as $Private<number>;
var c: $Trusted<number> = 42 as $Private<number>;
var d: $Trusted<number> = 42 as number;
var e: number = 42 as $Private<number>;
