let x: number extends string ? boolean : null;
let x: string | number extends string ? boolean : null;

let x: | number extends | number ? | number : | number;
let x: string | number extends string | number ? string | number : string | number;

let x: number extends string ? number extends string ? 1 : 2 : number extends string ? 1 : 2;
let x: (number extends string ? 1 : 2) extends (number extends string ? 1 : 2) ? boolean : null;

let x :
  (// comment 1
    (/* comment 2 */ number) extends string ? boolean : (null) /* trailing */)
