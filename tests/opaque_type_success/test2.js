//@flow

type CA = {|
  __type: 'A',
|};

type CB = {|
  __type: 'B',
|};

export opaque type C = CA | CB;

function t(applicationType: $PropertyType<C, '__type'>): number {
  switch (applicationType) {
    case 'A':
      return 42;
    case 'B':
      return 42;
    default:
      return 42;
  }
}
