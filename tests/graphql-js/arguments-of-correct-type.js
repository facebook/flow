// @flow

/*
 * Valid values
 */

// Good int value
gql`
  {
    complicatedArgs {
      intArgField(intArg: 2)
    }
  }
`;

// Good boolean value
gql`
  {
    complicatedArgs {
      booleanArgField(booleanArg: true)
    }
  }
`;

// Good string value
gql`
  {
    complicatedArgs {
      stringArgField(stringArg: "foo")
    }
  }
`;

// Good float value
gql`
  {
    complicatedArgs {
      floatArgField(floatArg: 1.1)
    }
  }
`;

// Int into Float
gql`
  {
    complicatedArgs {
      floatArgField(floatArg: 1)
    }
  }
`;

// Int into ID
gql`
  {
    complicatedArgs {
      # By default ID accepts only strings, as opposed to graphql-js which
      # allows both Int and String.
      idArgField(idArg: 1) # ID is a string
    }
  }
`;

// String into ID
gql`
  {
    complicatedArgs {
      idArgField(idArg: "someIdString")
    }
  }
`;

// Good enum value
gql`
  {
    dog {
      doesKnowCommand(dogCommand: SIT)
    }
  }
`;

// null into nullable type
gql`
  {
    complicatedArgs {
      intArgField(intArg: null)
    }
  }
`;

/*
 * Invalid String values
 */

// Int into String
gql`
  {
    complicatedArgs {
      stringArgField(stringArg: 1) # error
    }
  }
`;

// Float into String
gql`
  {
    complicatedArgs {
      stringArgField(stringArg: 1.0) # error
    }
  }
`;

// Boolean into String
gql`
     {
       complicatedArgs {
         stringArgField(stringArg: true) # error
       }
     }
`;

// Unquoted String into String
gql`
  {
    complicatedArgs {
      stringArgField(stringArg: BAR) # error
    }
  }
`;

/*
 * Invalid Int values
 */

// String into Int
gql`
  {
    complicatedArgs {
      intArgField(intArg: "3") # error
    }
  }
`;

// Big Int into Int
gql`
  {
    complicatedArgs {
      intArgField(intArg: 829384293849283498239482938) # error
    }
  }
`;

// Unquoted String into Int
gql`
  {
    complicatedArgs {
      intArgField(intArg: FOO) # error
    }
  }
`;

// Simple Float into Int
gql`
  {
    complicatedArgs {
      intArgField(intArg: 3.0) # error
    }
  }
`;

// Float into Int
gql`
  {
    complicatedArgs {
      intArgField(intArg: 3.333) # error
    }
  }
`;

/*
 * Invalid Float values
 */

// String into Float
gql`
  {
    complicatedArgs {
      floatArgField(floatArg: "3.333") # error
    }
  }
`;

// Boolean into Float
gql`
  {
    complicatedArgs {
      floatArgField(floatArg: true) # error
    }
  }
`;

// Unquoted into Float
gql`
  {
    complicatedArgs {
      floatArgField(floatArg: FOO) # error
    }
  }
`;


/*
 * Invalid Boolean value
 */

// Int into Boolean
gql`
  {
    complicatedArgs {
      booleanArgField(booleanArg: 2) # error
    }
  }
`;

// Float into Boolean
gql`
  {
    complicatedArgs {
      booleanArgField(booleanArg: 1.0) # error
    }
  }
`;

// String into Boolean
gql`
  {
    complicatedArgs {
      booleanArgField(booleanArg: "true") # error
    }
  }
`;

// Unquoted into Boolean
gql`
  {
    complicatedArgs {
      booleanArgField(booleanArg: TRUE) # error
    }
  }
`;


/*
 * Invalid ID value
 */

// Float into ID
gql`
  {
    complicatedArgs {
      idArgField(idArg: 1.0) # error
    }
  }
`;

// Boolean into ID
gql`
  {
    complicatedArgs {
      idArgField(idArg: true) # error
    }
  }
`;

// Unquoted into ID
gql`
  {
    complicatedArgs {
      idArgField(idArg: SOMETHING) # error
    }
  }
`;


/*
 * Invalid Enum value
 */

// Int into Enum
gql`
  {
    dog {
      doesKnowCommand(dogCommand: 2) # error
    }
  }
`;

// Float into Enum
gql`
  {
    dog {
      doesKnowCommand(dogCommand: 1.0) # error
    }
  }
`;

// String into Enum
gql`
  {
    dog {
      doesKnowCommand(dogCommand: "SIT") # error
    }
  }
`;

// Boolean into Enum
gql`
  {
    dog {
      doesKnowCommand(dogCommand: true) # error
    }
  }
`;

// Unknown Enum Value into Enum
gql`
  {
    dog {
      doesKnowCommand(dogCommand: JUGGLE) # error
    }
  }
`;

// Different case Enum Value into Enum
gql`
  {
    dog {
      doesKnowCommand(dogCommand: sit) # error
    }
  }
`;

/*
 * Valid List value
 */

// Good list value
gql`
  {
    complicatedArgs {
      stringListArgField(stringListArg: ["one", null, "two"])
    }
  }
`;

// Empty list value
gql`
  {
    complicatedArgs {
      stringListArgField(stringListArg: [])
    }
  }
`;

// Null value
gql`
  {
    complicatedArgs {
      stringListArgField(stringListArg: null)
    }
  }
`;

// Single value into List
gql`
  {
    complicatedArgs {
      stringListArgField(stringListArg: "one")
    }
  }
`;


/*
 * Invalid List value
 */

// Incorrect item type
gql`
  {
    complicatedArgs {
      stringListArgField(stringListArg: ["one", 2]) # expected String, found Int
    }
  }
`;

// Single value of incorrect type
gql`
  {
    complicatedArgs {
      stringListArgField(stringListArg: 1) # expected String, found Int
    }
  }
`;


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

// Incorrect value type
gql`
  {
    complicatedArgs {
      # 2 errors: expected Int, found String
      multipleReqs(req2: "two", req1: "one")
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

// Null value
gql`
  {
    complicatedArgs {
      multipleReqs(req1: null) # missing 'req2', bad value for 'req1'
    }
  }
`;


/*
 * Valid input object value
 */

// Optional arg, despite required field in type
gql`
  {
    complicatedArgs {
      complexArgField
    }
  }
`;

// Partial object, only required
gql`
  {
    complicatedArgs {
      complexArgField(complexArg: { requiredField: true })
    }
  }
`;

// Partial object, required field can be falsey
gql`
  {
    complicatedArgs {
      complexArgField(complexArg: { requiredField: false })
    }
  }
`;

// Partial object, including required
gql`
  {
    complicatedArgs {
      complexArgField(complexArg: { requiredField: true, intField: 4 })
    }
  }
`;

// Full object
gql`
  {
    complicatedArgs {
      complexArgField(complexArg: {
        requiredField: true,
        intField: 4,
        stringField: "foo",
        booleanField: false,
        stringListField: ["one", "two"]
      })
    }
  }
`;

// Full object with fields in different order
gql`
  {
    complicatedArgs {
      complexArgField(complexArg: {
        stringListField: ["one", "two"],
        booleanField: false,
        requiredField: true,
        stringField: "foo",
        intField: 4,
      })
    }
  }
`;


/*
 * Invalid input object value
 */

// Partial object, missing required
gql`
  {
    complicatedArgs {
      complexArgField(complexArg: { intField: 4 }) # expected Bool, found null
    }
  }
`;

// Partial object, invalid field type
gql`
  {
    complicatedArgs {
      complexArgField(complexArg: {
        stringListField: ["one", 2], # expected string, found '2'
        requiredField: true,
      })
    }
  }
`;

// Partial object, unknown field arg
gql`
  {
    complicatedArgs {
      complexArgField(complexArg: {
        requiredField: true,
        unknownField: "value" # unknown field
      })
    }
  }
`;

/*
 * Directive arguments
 */

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

// with directive with incorrect types
// gql`
//   {
//     dog @include(if: "yes") { # expected bool
//       name @skip(if: ENUM) # expected bool
//     }
//   }
// `;
