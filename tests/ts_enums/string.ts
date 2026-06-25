// String TS enum.
enum Status {
  Active = "active",
  Paused = "paused",
}

Status.Active satisfies "active"; // OK
Status.Active satisfies string; // OK

const s: Status = Status.Active; // OK

// DEVIATION FROM TS: TS treats string enums as nominal, so `const s2: Status =
// "active"` is an error there. Flow models the enum type as the union of member
// literals, so a matching string literal is accepted here. This is intentional.
const s2: Status = "active"; // OK

// A non-member string is still rejected, which proves `Status` (as a type) is the
// union of its member literals and not just `string`. This matches TS.
const s3: Status = "nope"; // ERROR
