interface Rest<T> {
  next(...[value]: [] | [T]): void;
  pair(...[first, second]: [number, string]): void;
}
type Destructured = (...[value]: [] | [number]) => void;
type Anonymous = (...[number, string]) => void;
