// @flow

var obj: {b?: number} = {b: 42};

export {obj};
export var optNum = obj.b;
export var optFunc = (p?: number) => p;
