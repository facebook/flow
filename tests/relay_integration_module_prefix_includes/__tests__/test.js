declare function graphql(Array<string>): string;

const fragment = graphql`
  fragment TestFragment on User {
    friends {
      count
    }
  }`;

(fragment: 1); // OK
(fragment: string); // ERROR

const query = graphql`
  query TestQuery {
    friends {
      count
    }
  }`;

(query: 2); // OK
(query: string); // ERROR
