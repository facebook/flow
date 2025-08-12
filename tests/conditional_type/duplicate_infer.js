type DuplicateInfer1 = [string, number] extends [infer Y extends string, infer Y extends number] ? Y : empty; // error: string = number
type DuplicateInfer2 = [string, number] extends [infer Y extends string, infer Y extends string] ? Y : empty;
type DuplicateInfer3 = [string, string] extends [infer Y extends string, infer Y extends string] ? Y : empty;
type DuplicateInfer4 = [string, number] extends [infer Y, infer Y] ? Y : empty;
type DuplicateInfer5 = [string, string] extends [infer Y, infer Y] ? Y : empty;

(1: DuplicateInfer1); // error: number ~> empty
(1: DuplicateInfer2); // error: number ~> empty
(1: DuplicateInfer3); // error: number ~> string
(1: DuplicateInfer4); // error: number ~> empty
(1: DuplicateInfer5); // error: number ~> string
