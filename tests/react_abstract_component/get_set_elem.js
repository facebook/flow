//@flow

declare var x: React$AbstractComponent<any, any, any>;
declare var y: string;
x[y]; // error
x[y] = y; // error: no indexed type
