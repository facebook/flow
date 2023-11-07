//@flow

export var {a} = 42 as any;

export var {'42': b} = 42 as any;

export var {[b]: c} = 42 as any;
