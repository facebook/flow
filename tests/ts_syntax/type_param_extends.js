type A<T extends string> = [T];
type B<out T extends string> = [T];
type C<T extends string = "hi"> = [T];
type D<out T extends string = "hi"> = [T];
