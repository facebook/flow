declare function graphql(Array<string>): string;

graphql`MissingFile`; // ERROR

const fragment = graphql`MyFragment`;

(fragment: number); // OK: we get the type from `foo.graphql.js`
(fragment: string); // ERROR
