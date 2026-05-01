/* basic */

let x : number = 1;

x as string; // error correctly

//$FlowFixMe
x as string; // error not suppressed, missing code

//$FlowFixMe[]
x as string; // error not suppressed, malformed code

// $FlowFixMe
x as string; // error not suppressed, missing code

/*

$FlowFixMe*/
x as string; // error not suppressed, missing code

//$FlowFixMe arithmetic isn't parsed as an error code
x as string; // error not suppressed, missing code

//$FlowFixMe[incompatible-type] asdfasdf
x as string; // error suppressed

//$FlowFixMe[incompatible-type][anythinghere][moregarbage]
x as string; // error suppressed

// $FlowFixMe [incompatible-type]
x as string; // error not suppressed, malformed code

// $FlowFixMe[arithmetic]
x as string; // error not suppressed + unused suppression

/* two codes */

let y : number | { x : number }  = 1;

y.x as string; // both errors appear

// $FlowFixMe
y.x as string; // errors not suppressed, missing code

//$FlowFixMe[incompatible-type]
y.x as string; // only the property access shows up

//$FlowFixMe[prop-missing]
y.x as string; // only the incompatibility shows up

//$FlowFixMe[incompatible-type]
//$FlowFixMe[prop-missing]
y.x as string; // errors suppressed

//foo
/*

$FlowFixMe[incompatible-type]




*/
//$FlowFixMe[prop-missing]
y.x as string; // errors suppressed

//$FlowFixMe[incompatible-type]

//$FlowFixMe[prop-missing]
y.x as string; // incompatibility shows up

//$FlowFixMe[incompatible-type]
/* interrupt */
//$FlowFixMe[prop-missing]
y.x as string; // incompatibility shows up

//$FlowFixMe[incompatible-type]
//$FlowFixMe[prop-missing]

y.x as string; // both

//$FlowFixMe[incompatible-type]
//$FlowFixMe[prop-missing]
//$FlowFixMe[arithmetic]
y.x as string; // errors suppressed, unused arithmetic

/* speculation */

let z : number = 3;

//$FlowFixMe
z as string | boolean; // errors not suppressed, missing code

//$FlowFixMe[incompatible-type]
z as string | boolean; // suppressed

//$FlowFixMe[prop-missing]
z as string | boolean; // error + unused suppression

/* two diff errors on same line */
declare function foo (x: string) : void;

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
any as {} & number as {bar:string}; // error

// $FlowFixMe
any as {} & number as {bar:string};  // error not suppressed, missing code

// $FlowFixMe[incompatible-type]
any as {} & number as {bar:string};  // no error

// $FlowFixMe[incompatible-type]
any as {} & number as {bar:string};  // error + unused suppression

// $FlowFixMe[prop-missing]
any as {} & number as {bar:string};  // error + unused suppression

/* Malformed */

//$FlowFixMe[incompatible-use, arithmetic]
x as string; // error + unused suppression

//$FlowFixMe[incompatible-use,arithmetic]
x as string; // error + unused suppression

//$FlowFixMe[incompatible-use]
x as string; // error + unused suppression

//$FlowFixMe[A]
x as number; // malformed

//$FlowFixMe[ ]
x as number; // malformed

//$FlowFixMe[<]
x as number; // malformed

//$FlowFixMe[>]
x as number; // malformed

//$FlowFixMe[*]
x as number; // malformed

//$FlowFixMe[_]
x as number; // malformed

//$FlowFixMe[1]
x as number; // malformed

//$FlowFixMe[0]
x as number; // malformed

/*$FlowFixMe[
]*/
x as number; // malformed

/* last in file */
//$FlowFixMe[incompatible-type] asdfasdf
x as string;
