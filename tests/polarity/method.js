type F<in X> = (x: X) => void;
type A<in X> = {
  x: <out X>(x: F<X>) => void; 
}
