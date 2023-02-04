//@flow
const is_string = (y: mixed): boolean %checks => {
    return typeof y === "string";
  }

  const is_bool = (y: mixed): boolean %checks => {
    return typeof y === "boolean";
  }

  const is_number = (y: mixed): boolean %checks => {
    return typeof y === "number";
  }

  // Feature check:
  const foo1 = function(x: string | Array<string>): string {
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

  // Same as above but refining an offset
  const bar = function(z: { f: string | Array<string>}): string {
    if (is_string(z.f)) {
      return z.f;
    } else {
      return z.f.join();
    }
  }

  const is_number_or_bool = (y: mixed): boolean %checks => {
    return is_number(y) || is_bool(y);
  }

  const baz = function(z: string | number): number {
    if (is_number_or_bool(z)) {
      return z;
    } else {
      return z.length;
    }
  }

  // Feature: multi params
  const multi_param = (w:mixed,x:mixed,y:mixed,z:mixed): boolean %checks => {
    return typeof z === "string";
  }

  const foo2 = function(x: string | Array<string>): string {
    if (multi_param("1", "2", "3", x)) {
      return x;
    } else {
      return x.join();
    }
  }

  const foo3 = function(a: mixed, b: mixed) {
    if (two_strings(a, b)) {
      from_two_strings(a, b);
    }
  }

  const two_strings = (x: mixed,y: mixed): boolean %checks => {
    return is_string(x) && is_string(y) ;
  }

  declare function from_two_strings(x: string, y: string): void;

  const two_strings_arrow = (x: mixed,y: mixed): boolean %checks =>
    is_string(x) && is_string(y);
