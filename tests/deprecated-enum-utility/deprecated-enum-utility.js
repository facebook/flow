/* @flow */

type T = $Enum<{A: string}>; // error
type S = $Keys<{A: string}>; // no error
