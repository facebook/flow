// @flow

declare function is_string(x: mixed): $StrP<0>;
declare function is_number(x: mixed): $NumP<0>;

// Feature check:
function foo(x: string | Array<string>): string {
  if (is_string(x)) {
    // The use of `is_string` as a conditional check
    // should guarantee the narrowing of the type of `x`
    // to string.
    return x;
  } else {
    // Accordingly the negation of the above check
    // guarantees that `x` here is an Array<string>
    return x.join();
  }
}
