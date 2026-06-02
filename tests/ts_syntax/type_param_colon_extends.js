type A<T: string> = [T];
type B<out T: string> = [T];
type C<T: string = "hi"> = [T];
type D<out T: string = "hi"> = [T];
