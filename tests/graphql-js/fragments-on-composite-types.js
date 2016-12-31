// @flow

// object is valid fragment type
gql`
  fragment validFragment on Dog {
    barks
  }
`;

// interface is valid fragment type
gql`
  fragment validFragment on Pet {
    name
  }
`;

// object is valid inline fragment type
gql`
  fragment validFragment on Pet {
    ... on Dog {
      barks
    }
  }
`;

// inline fragment without type is valid
gql`
  fragment validFragment on Pet {
    ... {
      name
    }
  }
`;

// union is valid fragment type
gql`
  fragment validFragment on CatOrDog {
    __typename
  }
`;

// scalar is invalid fragment type
gql`
  fragment scalarFragment on Boolean { # error
    bad
  }
`;

// enum is invalid fragment type
gql`
  fragment scalarFragment on FurColor { # error
    bad
  }
`;

// input object is invalid fragment type
gql`
  fragment inputFragment on ComplexInput { # error
    stringField
  }
`;

// scalar is invalid inline fragment type
gql`
  fragment invalidFragment on Pet { # error
    ... on String {
      barks
    }
  }
`;
