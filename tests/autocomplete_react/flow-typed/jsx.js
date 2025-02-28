declare opaque type CommonInstance;
declare opaque type DivInstance: CommonInstance;
declare opaque type SpanInstance: CommonInstance;
declare opaque type ImgInstance: CommonInstance;

type $JSXIntrinsics = {
  [string]: {
    props: any,
    instance: CommonInstance,
  },
  div: {
    props: {},
    instance: DivInstance,
  },
  span: {
    props: {},
    instance: SpanInstance,
  },
  img: {
    props: {},
    instance: ImgInstance,
  },
}
