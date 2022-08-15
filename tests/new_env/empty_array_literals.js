//@flow

declare var str: string;

const x0 = []; // error

const x1 = { f: [] }; // TODO should error due to missing annotation

const x2: any = { f: [] };

var x3: any = 1;
x3 = { f: [] };

var x4 = null;
({ x4 } = { x4: [] }); // TODO should error due to provider position

const x5: { [string]: Array<number> } = {};
x5[str] = [];
