type A<T: string> = [T];
type B<+T: string> = [T];
type C<T: string = "hi"> = [T];
type D<+T: string = "hi"> = [T];
