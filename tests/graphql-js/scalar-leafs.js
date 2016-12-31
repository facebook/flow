// @flow

// valid scalar selection
gql`
  fragment scalarSelection on Dog {
    barks
  }
`;

// object type missing selection
gql`
  query directQueryOnObjectWithoutSubFields {
    human # error
    # Currently flow allows queries with no selection for Relay. So we add a
    # field with selection to trigger validation.
    cat { name }
  }
`;

// interface type missing selection
gql`
  {
    human { pets } # error
  }
`;

// valid scalar selection with args
gql`
  fragment scalarSelectionWithArgs on Dog {
    doesKnowCommand(dogCommand: SIT)
  }
`;

// scalar selection not allowed on Boolean
gql`
  fragment scalarSelectionsNotAllowedOnBoolean on Dog {
    barks { sinceWhen } # error
  }
`;

// scalar selection not allowed on Enum
gql`
  fragment scalarSelectionsNotAllowedOnEnum on Cat {
    furColor { inHexdec } # error
  }
`;

// scalar selection not allowed with args
gql`
  fragment scalarSelectionsNotAllowedWithArgs on Dog {
    doesKnowCommand(dogCommand: SIT) { sinceWhen } # error
  }
`;

// Scalar selection not allowed with directives
gql`
  fragment scalarSelectionsNotAllowedWithDirectives on Dog {
    name @include(if: true) { isAlsoHumanName } # error
  }
`;

// Scalar selection not allowed with directives and args
gql`
  fragment scalarSelectionsNotAllowedWithDirectivesAndArgs on Dog {
    doesKnowCommand(dogCommand: SIT) @include(if: true) { sinceWhen } # error
  }
`;
