type $JSXIntrinsic<T> = Class<ReactComponent<T,T,mixed>>;

type $JSXIntrinsics = {
  div: $JSXIntrinsic<{id: string}>,
  span: $JSXIntrinsic<{id: string, class: string}>,
};
