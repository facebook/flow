declare function graphql(x: Array<string>): string;

graphql`query MissingFile { foo }`; // ERROR

const fragment = graphql`
  fragment MyFragment on User {
    friends {
      count
    }
  }`;

fragment as 1; // OK
fragment as string; // ERROR

const query = graphql`
  query MyQuery {
    friends {
      count
    }
  }`;

query as 2; // OK
query as string; // ERROR

const mutation = graphql`
  mutation MyMutation($x: String) {
    friends(name: $x) {
      count
    }
  }`;

mutation as 3; // OK
mutation as string; // ERROR

const subscription = graphql`
  subscription MySubscription($x: String) {
    friends(name: $x) {
      count
    }
  }`;

subscription as 4; // OK
subscription as string; // ERROR

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

fragmentWithComment as 1; // OK
fragmentWithComment as string; // ERROR

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

fragmentWithCommas as 1; // OK
fragmentWithCommas as string; // ERROR

graphql``; // ERROR
graphql`query MyQuery {${fragment}}`; // ERROR
graphql`boop MyBoop {}`; // ERROR
graphql`
  query MyQuery {
    foo {
      query
      bar
    }
  }`; // OK
