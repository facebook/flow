// Flow object types still require an explicit comma or semicolon between members.
type T = {
  a: number
  b: string // ERROR
};
