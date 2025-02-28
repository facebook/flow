declare opaque type CommonInstance;
declare opaque type DivInstance: CommonInstance;
declare opaque type ButtonInstance: CommonInstance;
declare opaque type AInstance: CommonInstance;

type $JSXIntrinsics = {
  [string]: {
    props: any,
    instance: CommonInstance,
  },
  div: {
    props: {...},
    instance: DivInstance,
  },
  button: {
    props: {...},
    instance: ButtonInstance,
  },
  a: {
    props: {...},
    instance: AInstance,
  },
}
