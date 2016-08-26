// @flow

const basic = Relay.QL`
  fragment on Person {
    name
  }
`;

const que = Relay.QL`
  query {
    person {
      id
      ${basic}
      ${require('./frag')}
      kappa # doesn't exist
    }
    me # doesn't exist
  }
`;

declare function toObj<T>(props: T): $RelayProps<T>;

const obj = toObj({
  query: () => que,
});

const {person} = obj.query;
if (person) {
  person.id;
  person.name;
  person.mass; // error: Didn't request field `mass`.
}
