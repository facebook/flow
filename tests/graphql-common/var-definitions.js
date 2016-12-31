// @flow

// double non-null annotation
gql`
  query($id: ID!!) { # error
    __typename
  }
`;
