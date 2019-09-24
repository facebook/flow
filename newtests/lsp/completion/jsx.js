// @flow

declare var React: {
  createElement: any
};

type Props = {a :number}

class C {
  props: Props
}
  // <- space
<C   />// <- space
<C. />
C.
