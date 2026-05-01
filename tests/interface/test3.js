interface I { x: number, y : string }
interface J { y : number }
interface K extends I, J { x: string } // error: x is number in I
function foo(k: K) {
  k.x as number; // error: x is string in K
  k.y as number; // error: y is string in I
}
