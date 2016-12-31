// @flow

// known type names are valid
gql`
  query Foo($var: String, $required: [String!]!) {
    human {
      pets { ... on Pet { name }, ...PetFields, ... { name } }
    }
  }
  fragment PetFields on Pet {
    name
  }
`;

// unknown type names are invalid
gql`
  query Foo($var: JumbledUpLetters) { # error
    human {
      name
      pets { ... on Badger { name }, ...PetFields } # error
    }
  }
  fragment PetFields on Peettt { # error
    name
  }
`;

// ignores type definitions
// gql`
//   type NotInTheSchema {
//     field: FooBar
//   }
//   interface FooBar {
//     field: NotInTheSchema
//   }
//   union U = A | B
//   input Blob {
//     field: UnknownType
//   }
//   query Foo($var: NotInTheSchema) {
//     user(id: $var) {
//       id
//     }
//   }
// `;
