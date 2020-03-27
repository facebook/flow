//@flow
type X<T> = Array<T> | X<Array<T>>
  
([] : X<number>)

type Y<T> = $ReadOnlyArray<T> | Y<$ReadOnlyArray<T>>
  
([] : Y<number>)

type Z<T> = [T] | Z<[T]>

([] : Z<number>)
