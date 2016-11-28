// @flow

const query = gql`
  query {
    me {
      id
    }
  }
`;

const frag = gql`
  fragment on Query {
    me {
      abcd
      name
      ... on Node { id }
    }
    ${null} # ignored for now
    ... {
      version
      ext
    }
  }
`;
