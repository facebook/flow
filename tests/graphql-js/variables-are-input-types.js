// @flow

// input types are valid
gql`
  query Foo($a: String, $b: [Boolean!]!, $c: ComplexInput) {
    __typename
  }
`;

// output types are invalid
gql`
  query Foo($a: Dog, $b: [[CatOrDog!]]!, $c: Pet) { # 3 errors
    __typename
  }
`;
