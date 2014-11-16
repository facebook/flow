function resolve(result) { }
function reject(error) { }

var p = new Promise(
    function(resolve, reject) {
        resolve(0);
    }
);
var q = p.then(
    function(result) { var x:string = result; return x; },
    function(error) { return ""; }
);
var r = q.then(
    function(result) { var x:number = result; return Promise.resolve(x); },
    function(error) { return 0; }
);
