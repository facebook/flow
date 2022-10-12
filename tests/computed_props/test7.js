const obj: {|m(): number, x: number|} = {x: 0, m() { return obj.x }}
const x: string = obj['m'](); // error, number ~> string

const arr = [function(this: Array<() => number>) { return this.length }];
const y: string = arr[0](); // error: number ~> string
