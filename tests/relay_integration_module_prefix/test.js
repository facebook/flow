declare function graphql(Array<string>): string;

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
