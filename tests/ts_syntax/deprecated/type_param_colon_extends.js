type A<T: string> = [T]; // ERROR
type B<+T: string> = [T]; // ERROR
type C<T: string = "hi"> = [T]; // ERROR
type D<+T: string = "hi"> = [T]; // ERROR
