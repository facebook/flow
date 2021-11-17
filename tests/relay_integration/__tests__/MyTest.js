declare function graphql(Array<string>): string;

// We ignore the `__tests__` directory with the `relay_integration.excludes` option
const query = graphql`
  fragment MyTestQuery on User {
    friends {
      count
    }
  }`;

(query: string); // OK
