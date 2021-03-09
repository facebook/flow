const obj = {x: 0, m() { return this.x }}
const x: string = obj['m'](); // error, number ~> string

const arr = [function() { return this.length }];
const y: string = arr[0](); // error: number ~> string
