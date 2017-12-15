/* @flow */

//never enabled suppression; not added by --include-suppressed
var a: ?number = 0;
if (a){}

//flowlint sketchy-null:warn
//flowlint-next-line sketchy-null:off
var b: ?number = 0;
if (b){} //warning included by --include-suppressed (reported as an error)

//flowlint sketchy-null:error
//flowlint-next-line sketchy-null:off
var c: ?number = 0;
if (c){} //error included by --include-suppressed
