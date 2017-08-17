type $JSXIntrinsic<T> = any; // TODO: next diff = {props: T};

type $JSXIntrinsics = {
  div: $JSXIntrinsic<{id: string}>,
  span: $JSXIntrinsic<{id: string, class: string}>,
};
