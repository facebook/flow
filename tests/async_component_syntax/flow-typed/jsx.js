declare opaque type CommonInstance;

type $JSXIntrinsics = {
  [string]: {
    props: any,
    instance: CommonInstance,
  },
  div: {
    props: {children?: React$Node, ...},
    instance: CommonInstance,
  },
}
