// @flow

declare class Mutation<T> {
  static fragments: T;
  props: $RelayProps<T>;
}

class MyMutation extends Mutation {
  static fragments = {
    person: () => Relay.QL`
      fragment on Person {
        name
      }
    `,
  };

  getInput() {
    const {person} = this.props;
    person && person.id; // error: Didn't request field.
    person && person.name;
  }
}
