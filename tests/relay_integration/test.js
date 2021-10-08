declare function graphql(Array<string>): string;

graphql`query MissingFile { foo }`; // ERROR

const fragment = graphql`
  fragment MyFragment on User {
    friends {
      count
    }
  }`;

(fragment: 1); // OK
(fragment: string); // ERROR

const query = graphql`
  query MyQuery {
    friends {
      count
    }
  }`;

(query: 2); // OK
(query: string); // ERROR

const mutation = graphql`
  mutation MyMutation($x: String) {
    friends(name: $x) {
      count
    }
  }`;

(mutation: 3); // OK
(mutation: string); // ERROR

const subscription = graphql`
  subscription MySubscription($x: String) {
    friends(name: $x) {
      count
    }
  }`;

(subscription: 4); // OK
(subscription: string); // ERROR

// Ignore comments in GraphQL
const fragmentWithComment = graphql`
  # comment
  fragment
  # absolutely ridiculous to have a comment here
  MyFragment on User {
    friends {
      count
    }
  }`;

(fragmentWithComment: 1); // OK
(fragmentWithComment: string); // ERROR

// Commas are ignored!
const fragmentWithCommas = graphql`
  ,
  fragment
    ,
  MyFragment on User {
    friends {
      count
    }
  }`;

(fragmentWithCommas: 1); // OK
(fragmentWithCommas: string); // ERROR
