/* basic */

let x : number = 1;

(x : string); // error correctly

//$FlowFixMe
(x : string); // error not suppressed, missing code

//$FlowFixMe[]
(x : string); // error not suppressed, malformed code

// $FlowFixMe
(x : string); // error not suppressed, missing code

/*

$FlowFixMe*/
(x: string); // error not suppressed, missing code

//$FlowFixMe arithmetic isn't parsed as an error code
(x : string); // error not suppressed, missing code

//$FlowFixMe[incompatible-type] asdfasdf
(x : string); // error suppressed

//$FlowFixMe[incompatible-type][anythinghere][moregarbage]
(x : string); // error suppressed

// $FlowFixMe [incompatible-type]
(x : string); // error not suppressed, malformed code

// $FlowFixMe[arithmetic]
(x : string); // error not suppressed + unused suppression

/* two codes */

let y : number | { x : number }  = 1;

(y.x : string); // both errors appear

// $FlowFixMe
(y.x : string); // errors not suppressed, missing code

//$FlowFixMe[incompatible-type]
(y.x : string); // only the property access shows up

//$FlowFixMe[prop-missing]
(y.x : string); // only the incompatibility shows up

//$FlowFixMe[incompatible-type]
//$FlowFixMe[prop-missing]
(y.x : string); // errors suppressed

//foo
/*

$FlowFixMe[incompatible-type]




*/
//$FlowFixMe[prop-missing]
(y.x : string); // errors suppressed

//$FlowFixMe[incompatible-type]

//$FlowFixMe[prop-missing]
(y.x : string); // incompatibility shows up

//$FlowFixMe[incompatible-type]
/* interrupt */
//$FlowFixMe[prop-missing]
(y.x : string); // incompatibility shows up

//$FlowFixMe[incompatible-type]
//$FlowFixMe[prop-missing]

(y.x : string); // both

//$FlowFixMe[incompatible-type]
//$FlowFixMe[prop-missing]
//$FlowFixMe[arithmetic]
(y.x : string); // errors suppressed, unused arithmetic

/* speculation */

let z : number = 3;

//$FlowFixMe
(z : string | boolean); // errors not suppressed, missing code

//$FlowFixMe[incompatible-type]
(z : string | boolean); // suppressed

//$FlowFixMe[prop-missing]
(z : string | boolean); // error + unused suppression

/* two diff errors on same line */
declare function foo (string) : void;

// $FlowFixMe
foo(3, 4); // error not suppressed, missing code

// $FlowFixMe[incompatible-type]
// $FlowFixMe[extra-arg]
foo(3, 4); // no error

// $FlowFixMe[incompatible-type]
foo(3, 4); // one error

// $FlowFixMe[extra-arg]
foo(3, 4); // one error

/* complex multi-codes */

declare var any: any;
((any: {} & number): {bar:string}); // error

// $FlowFixMe
((any: {} & number): {bar:string});  // error not suppressed, missing code

// $FlowFixMe[incompatible-type]
((any: {} & number): {bar:string});  // no error

// $FlowFixMe[incompatible-type]
((any: {} & number): {bar:string});  // error + unused suppression

// $FlowFixMe[prop-missing]
((any: {} & number): {bar:string});  // error + unused suppression

/* Malformed */

//$FlowFixMe[incompatible-use, arithmetic]
(x : string); // error + unused suppression

//$FlowFixMe[incompatible-use,arithmetic]
(x : string); // error + unused suppression

//$FlowFixMe[incompatible-use]
(x : string); // error + unused suppression

//$FlowFixMe[A]
(x : number); // malformed

//$FlowFixMe[ ]
(x : number); // malformed

//$FlowFixMe[<]
(x : number); // malformed

//$FlowFixMe[>]
(x : number); // malformed

//$FlowFixMe[*]
(x : number); // malformed

//$FlowFixMe[_]
(x : number); // malformed

//$FlowFixMe[1]
(x : number); // malformed

//$FlowFixMe[0]
(x : number); // malformed

/*$FlowFixMe[
]*/
(x : number); // malformed

/* last in file */
//$FlowFixMe[incompatible-type] asdfasdf
(x : string);
