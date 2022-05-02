//@flow

function f1(): void {}
var f1: number = 1; // error [name-already-bound]

function f2(): void {}
let f2: number = 2; // error [name-already-bound]

function f3(): void {}
const f3: number = 3; // error [name-already-bound]

function f4(): void {}
var f4 = 4; // error [name-already-bound]

declare function f5(): void;
declare function f5(): void;
function f5(): void {};
function f5(): void {}; // TODO: error [name-already-bound]
var f5 = 5; // error [name-already-bound]

class c1 {}
var c1: number = 1; // error [name-already-bound]

class c2 {}
var c2 = 2; // error [name-already-bound]
