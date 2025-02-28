type $JSXIntrinsic<T> = {props: T, instance: any, ...};

type $JSXIntrinsics = {
  div: $JSXIntrinsic<{id: string, [StringPrefix<'data-'>]: string}>,
  span: $JSXIntrinsic<{id: string, class: string, ...}>,
  ...
};
