type DuplicatInfer1 = [string, number] extends [infer Y extends string, infer Y extends number] ? Y : empty; // error: string = number
type DuplicatInfer2 = [string, number] extends [infer Y extends string, infer Y extends string] ? Y : empty;
type DuplicatInfer3 = [string, string] extends [infer Y extends string, infer Y extends string] ? Y : empty;
type DuplicatInfer4 = [string, number] extends [infer Y, infer Y] ? Y : empty;
type DuplicatInfer5 = [string, string] extends [infer Y, infer Y] ? Y : empty;

(1: DuplicatInfer1); // error: number ~> empty
(1: DuplicatInfer2); // error: number ~> empty
(1: DuplicatInfer3); // error: number ~> string
(1: DuplicatInfer4); // error: number ~> empty
(1: DuplicatInfer5); // error: number ~> string
