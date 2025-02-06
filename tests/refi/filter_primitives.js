declare var maybeStr: ?'a';
declare var maybeNum: ?1;
declare var maybeBool: ?true;
declare var maybeBigInt: ?1n;

if (maybeStr != null) {
    'a' as typeof maybeStr; // ok
    'b' as typeof maybeStr; // error 'b' ~> 'a'
}

if (maybeNum != null) {
    1 as typeof maybeNum; // ok
    2 as typeof maybeNum; // error 2 ~> 1
}

if (maybeBool != null) {
    true as typeof maybeBool; // ok
    false as typeof maybeBool; // error false ~> true
}

if (maybeBigInt != null) {
    1n as typeof maybeBigInt; // ok
    2n as typeof maybeBigInt; // error 2n ~> 1n
}
