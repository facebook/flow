// @flow

// Object field selection
gql`
  fragment objectFieldSelection on Dog {
    __typename
    name
  }
`;

// Aliased object field selection
gql`
  fragment aliasedObjectFieldSelection on Dog {
    tn : __typename
    otherName : name
  }
`;

// Interface field selection
gql`
  fragment interfaceFieldSelection on Pet {
    __typename
    name
  }
`;

// Aliased interface field selection
gql`
  fragment interfaceFieldSelection on Pet {
    otherName : name
  }
`;

// Lying alias selection
gql`
  fragment lyingAliasSelection on Dog {
    name : nickname
  }
`;

// Ignores fields on unknown type
gql`
  fragment unknownSelection on UnknownType { # only error for unknown type
    unknownField
  }
`;

// reports errors when type is known again
// gql`
//   fragment typeKnownAgain on Pet {
//     unknown_pet_field {
//       ... on Cat {
//         unknown_cat_field
//       }
//     }
//   }
// `;

// Field not defined on fragment
gql`
  fragment fieldNotDefined on Dog {
    meowVolume # field not defined
  }
`;

// Ignores deeply unknown field
gql`
  fragment deepFieldNotDefined on Dog {
    unknown_field { # error only here
      deeper_unknown_field
    }
  }
`;

// Sub-field not defined
gql`
  fragment subFieldNotDefined on Human {
    pets {
      unknown_field # field not defined
    }
  }
`;

// Field not defined on inline fragment
gql`
  fragment fieldNotDefined on Pet {
    ... on Dog {
      meowVolume # field not defined
    }
  }
`;

// Aliased field target not defined
gql`
  fragment aliasedFieldTargetNotDefined on Dog {
    volume : mooVolume # field not defined
  }
`;

// Aliased lying field target not defined
gql`
  fragment aliasedLyingFieldTargetNotDefined on Dog {
    barkVolume : kawVolume # field not defined
  }
`;

// Not defined on interface
gql`
  fragment notDefinedOnInterface on Pet {
    tailLength # field not defined
  }
`;

// Defined on implementors but not on interface
gql`
  fragment definedOnImplementorsButNotInterface on Pet {
    nickname # field not defined
  }
`;

// Meta field selection on union
gql`
  fragment directFieldSelectionOnUnion on CatOrDog {
    __typename
  }
`;

// Direct field selection on union
gql`
  fragment directFieldSelectionOnUnion on CatOrDog {
    directField # direct selection on union
  }
`;

// Defined on implementors queried on union
gql`
  fragment definedOnImplementorsQueriedOnUnion on CatOrDog {
    name # direct selection on union
  }
`;

// valid field in inline fragment
gql`
  fragment objectFieldSelection on Pet {
    ... on Dog {
      name
    }
    ... {
      name
    }
  }
`;
