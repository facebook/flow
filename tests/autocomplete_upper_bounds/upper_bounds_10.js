//@flow

declare var f : {|
  red: number,
  orange: number,
  yellow: number,
  green: number,
  blue: number,
  indigo: number,
  violet: number,
|} => void;

let subset = {
  red: 1,
  green: 2,
  violet: 3,
}

f({ orange: 1,   , ...subset
//             ^
