// @flow

const query = Relay.QL`
  query {
    me {
      id
    }
  }
`;

const frag = Relay.QL`
  fragment on Query {
    me {
      abcd
      name
      ... on Node { id }
    }
    ${null} # ignored for now
    ... {
      version
    }
  }
`;
