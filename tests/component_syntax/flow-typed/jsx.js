declare opaque type CommonInstance;
declare opaque type AInstance: CommonInstance;
declare opaque type MetaInstance: CommonInstance;
declare opaque type InputInstance: CommonInstance;

type $JSXIntrinsics = {
  [string]: {
    props: any,
    instance: CommonInstance,
  },
  a: {
    props: {children?: React$Node, ...},
    instance: AInstance,
  },
  div: {
    props: {children?: React$Node, ...},
    instance: CommonInstance,
  },
  span: {
    props: {children?: React$Node, ...},
    instance: CommonInstance,
  },
  input: {
    props: {children?: React$Node, ...},
    instance: InputInstance,
  },
  meta: {
    props: {children?: React$Node, [StringPrefix<'data-'>]: string | number},
    instance: MetaInstance,
  },
}
