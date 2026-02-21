type A<T: string> = [T]; // ERROR
type B<+T: string> = [T]; // ERROR
type C<T: string = "hi"> = [T]; // ERROR
type D<+T: string = "hi"> = [T]; // ERROR

export function intersectArrays<T>(arr1: Array<T>, arr2: Array<T>): Array<T> {
  const set1 = new Set(arr1);
  return arr2.filter(item => set1.has(item));
}
