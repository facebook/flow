type A<T extends string> = [T];
type B<+T extends string> = [T];
type C<T extends string = "hi"> = [T];
type D<+T extends string = "hi"> = [T];
