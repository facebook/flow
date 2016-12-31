// @flow

// unique variable names
gql`
  query A($x: Int, $y: String) { __typename }
  query B($x: String, $y: Int) { __typename }
`;

// duplicate variable names
gql`
  query A($x: Int, $x: Int, $x: String) { __typename } # 2 errors
  query B($x: String, $x: Int) { __typename } # error
  query C($x: Int, $x: Int) { __typename } # error
`;
