declare function graphql(x: Array<string>): string;

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
