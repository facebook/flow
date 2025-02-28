declare opaque type CommonInstance;

type $JSXIntrinsics = {
  [string]: {
    props: {
      children?: React.Node,
    },
    instance: CommonInstance,
  },
}
