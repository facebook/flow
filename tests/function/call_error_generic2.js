declare function bar1<TKey extends ?{+$data?: unknown, ...}>(key: TKey): void;

function bar2(fragmentRef?: {...}) {
  bar1(fragmentRef);
}

function foo(props: { userRef?: {...}, ... }) {
  const userRef = props.userRef;
  bar2(userRef);
}
