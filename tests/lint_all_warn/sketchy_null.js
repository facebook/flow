// @flow

declare const x: ?string;

//flowlint-next-line sketchy-null:error
if (x) { } // This should be an error

if (x) { } // This should be a warning
