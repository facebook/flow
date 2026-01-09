// @flow

type Rec = null | Rec;

type State = {
  data: ?Rec,
};

declare function useState<S>(x: S): $ReactDeepReadOnly<S>;

function Foo(s: State) {
  const state = useState<State>(s);
  const data = state.data;
}
