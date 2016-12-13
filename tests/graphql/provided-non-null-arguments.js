// @flow

/*
 * Valid non-nullable value
 */

// Arg on optional arg
gql`
  {
    dog {
      isHousetrained(atOtherHomes: true)
    }
  }
`;

// No Arg on optional arg
gql`
  {
    dog {
      isHousetrained
    }
  }
`;

// Multiple args
gql`
  {
    complicatedArgs {
      multipleReqs(req1: 1, req2: 2)
    }
  }
`;

// Multiple args reverse order
gql`
  {
    complicatedArgs {
      multipleReqs(req2: 2, req1: 1)
    }
  }
`;

// No args on multiple optional
gql`
  {
    complicatedArgs {
      multipleOpts
    }
  }
`;

// One arg on multiple optional
gql`
  {
    complicatedArgs {
      multipleOpts(opt1: 1)
    }
  }
`;

// Second arg on multiple optional
gql`
  {
    complicatedArgs {
      multipleOpts(opt2: 1)
    }
  }
`;

// Multiple reqs on mixedList
gql`
  {
    complicatedArgs {
      multipleOptAndReq(req1: 3, req2: 4)
    }
  }
`;

// Multiple reqs and one opt on mixedList
gql`
  {
    complicatedArgs {
      multipleOptAndReq(req1: 3, req2: 4, opt1: 5)
    }
  }
`;

// All reqs and opts on mixedList
gql`
  {
    complicatedArgs {
      multipleOptAndReq(req1: 3, req2: 4, opt1: 5, opt2: 6)
    }
  }
`;


/*
 * Invalid non-nullable value
 */

// Missing one non-nullable argument
gql`
  {
    complicatedArgs {
      multipleReqs(req2: 2) # missing 'req1'
    }
  }
`;

// Missing multiple non-nullable arguments
gql`
  {
    complicatedArgs {
      multipleReqs # missing 'req1' and 'req2'
    }
  }
`;

// Incorrect value and missing argument
gql`
  {
    complicatedArgs {
      multipleReqs(req1: "one") # missing 'req2', bad value for 'req1'
    }
  }
`;

/*
 * Directive arguments
 */

// ignores unknown directives
// gql`
//   {
//     dog @unknown
//   }
// `;

// with directives of valid types
// gql`
//   {
//     dog @include(if: true) {
//       name
//     }
//     human @skip(if: false) {
//       name
//     }
//   }
// `;

// with directive with missing types
// gql`
//   {
//     dog @include { # missing argument 'if'
//       name @skip # missing argument 'if'
//     }
//   }
// `;
