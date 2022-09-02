/* @flow */
var a = [];
for (var i = 0; i < 10; ++i) {
    if (i % 2 == 0) { a[i] = 0; }
    else { a[i] = ''; };
}

// annotations suffice to unblock the access constraint as well, so only
// uncalled internal functions will not find a type error, which is acceptable
// behavior as such functions are dead code.
function baz(i:number): string { return a[i]; }
