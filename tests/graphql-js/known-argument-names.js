// @flow

// single arg is known
gql`
  fragment argOnRequiredArg on Dog {
    doesKnowCommand(dogCommand: SIT)
  }
`;

// multiple args are known
gql`
  fragment multipleArgs on ComplicatedArgs {
    multipleReqs(req1: 1, req2: 2)
  }
`;

// ignores args of unknown fields
gql`
  fragment argOnUnknownField on Dog {
    unknownField(unknownArg: SIT) # unknown field
  }
`;

// multiple args in reverse order are known
gql`
  fragment multipleArgsReverseOrder on ComplicatedArgs {
    multipleReqs(req2: 2, req1: 1)
  }
`;

// no args on optional arg
gql`
  fragment noArgOnOptionalArg on Dog {
    isHousetrained
  }
`;

// args are known deeply
gql`
  {
    dog {
      doesKnowCommand(dogCommand: SIT)
    }
    human {
      pets {
        ... on Dog {
          doesKnowCommand(dogCommand: SIT)
        }
      }
    }
  }
`;

// directive args are known
// gql`
//   {
//     dog @skip(if: true)
//   }
// `;

// undirective args are invalid
// gql`
//   {
//     dog @skip(unless: true) # unknown arg `unless`
//   }
// `;

// invalid arg name
gql`
  fragment invalidArgName on Dog {
    doesKnowCommand(unknown: true) # error
  }
`;

// unknown args amongst known args
gql`
  fragment oneGoodArgOneInvalidArg on Dog {
    doesKnowCommand(whoknows: 1, dogCommand: SIT, unknown: true) # 2 errors
  }
`;

// unknown args deeply
gql`
  {
    dog {
      doesKnowCommand(unknown: true) # error
    }
    human {
      pets {
        ... on Dog {
          doesKnowCommand(unknown: true) # error
        }
      }
    }
  }
`;
