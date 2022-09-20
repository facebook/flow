//@flow

function f() {
  return 1;
}

var ya: number;
(ya: number)



const externalProps : {s: ?{field: string}} = {s : {field : "foo"}}
function mandatory(s : { field: string} ){ }

externalProps.s && modifyProps() && mandatory(externalProps.s);

function modifyProps(){
    externalProps.s = null;
    return true;
}

let x: number | string = 42;
let y: number | string = 42;

function havoc() {
    x = 42;
}

(typeof x === 'string') && havoc() && (x: string);
(typeof y === 'string') && havoc() && (y: string);
