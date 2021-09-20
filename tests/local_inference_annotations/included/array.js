//@flow
const annotated: any = [
  (x) => 3,
  (x) => {
    const a = (x) => 3; // Missing annot
    return (x) => 3;
  },
  ...[
    (x) => 3,
    (x) => {
      const b = (x) => 3; // Missing annot
      return (x) => 3;
    },
  ],
]
