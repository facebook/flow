/***
 * Nonstrict prop testing and refinements on exact object types
 * @flow
 */

//
// property test on object type
//

type Person = { first: string, last: string };

function prop_test_inexact(p: Person): string {
  if (p.xxx) {     // error, prop existence test on inexact type
    return p.xxx;
  }
  return p.first;
}

function prop_test_exact(p: $Exact<Person>): string {
  if (p.xxx) {     // ok to test for prop existence on exact types
    return p.xxx;  // ok currently, but should be reachability error
  }
  return p.first;
}

//
// property test on union of object types
//

type Address = { city: string, state: string };

function prop_test_inexact_union(pc: Person | Address): string {
  if (pc.first) {       // error, prop existence test on union of inexact types
    return pc.last;     // error, last not found on Address
  }
  return pc.state;      // error, state not found on Person
}

function prop_test_exact_union(pc: $Exact<Person> | $Exact<Address>): string {
  if (pc.first) {       // ok, union of exact types
    return pc.last;     // ok, refined to $Exact<Person>
  }
  return pc.state;      // error, since (pc: $Exact<Person>).first may be ""
}

//
// property test on union of object types (always truthy)
//

// Note: bug defeats prop existence refinements when prop is typed
// with an alias. Tracked by #12609525

// type Bundle1 = { person: Person, extra1: string };
// type Bundle2 = { address: Address, extra2: string };

type Bundle1 = { person: { first: string, last: string }, extra1: string };
type Bundle2 = { address: { city: string, state: string }, extra2: string };

function prop_test_exact_union_2(b: $Exact<Bundle1> | $Exact<Bundle2>): string {
  if (b.person) {       // ok
    return b.extra1;    // ok, refined to $Exact<Bundle1>
  }
  return b.extra2;      // ok, refined to $Exact<Bundle2>
}
