// @flow

// of the same object
// gql`
//   fragment objectWithinObject on Dog { ...dogFragment }
//   fragment dogFragment on Dog { barkVolume }
// `;

// of the same object with inline fragment
gql`
  fragment objectWithinObjectAnon on Dog { ... on Dog { barkVolume } }
`;

// object into an implemented interface
// gql`
//   fragment objectWithinInterface on Pet { ...dogFragment }
//   fragment dogFragment on Dog { barkVolume }
// `;

// object into containing union
// gql`
//   fragment objectWithinUnion on CatOrDog { ...dogFragment }
//   fragment dogFragment on Dog { barkVolume }
// `;

// union into contained object
// gql`
//   fragment unionWithinObject on Dog { ...catOrDogFragment }
//   fragment catOrDogFragment on CatOrDog { __typename }
// `;

// union into overlapping interface
// gql`
//   fragment unionWithinInterface on Pet { ...catOrDogFragment }
//   fragment catOrDogFragment on CatOrDog { __typename }
// `;

// union into overlapping union
// gql`
//   fragment unionWithinUnion on DogOrHuman { ...catOrDogFragment }
//   fragment catOrDogFragment on CatOrDog { __typename }
// `;

// interface into implemented object
// gql`
//   fragment interfaceWithinObject on Dog { ...petFragment }
//   fragment petFragment on Pet { name }
// `;

// interface into overlapping interface
// gql`
//   fragment interfaceWithinInterface on Pet { ...beingFragment }
//   fragment beingFragment on Being { name }
// `;

// interface into overlapping interface in inline fragment
// gql`
//   fragment interfaceWithinInterface on Pet { ... on Being { name } }
// `;

// interface into overlapping union
// gql`
//   fragment interfaceWithinUnion on CatOrDog { ...petFragment }
//   fragment petFragment on Pet { name }
// `;

// different object into object
// gql`
//   fragment invalidObjectWithinObject on Cat { ...dogFragment } # error
//   fragment dogFragment on Dog { barkVolume }
// `;

// different object into object in inline fragment
gql`
  fragment invalidObjectWithinObjectAnon on Cat {
    ... on Dog { barkVolume } # error
  }
`;

// object into not implementing interface
// gql`
//   fragment invalidObjectWithinInterface on Pet { ...humanFragment } # error
//   fragment humanFragment on Human { pets { name } }
// `;

// object into not containing union
// gql`
//   fragment invalidObjectWithinUnion on CatOrDog { ...humanFragment } # error
//   fragment humanFragment on Human { pets { name } }
// `;

// union into not contained object
// gql`
//   fragment invalidUnionWithinObject on Human { ...catOrDogFragment } # error
//   fragment catOrDogFragment on CatOrDog { __typename }
// `;

// union into non overlapping interface
// gql`
//   fragment invalidUnionWithinInterface on Pet { ...humanOrAlienFragment } # error
//   fragment humanOrAlienFragment on HumanOrAlien { __typename }
// `;

// union into non overlapping union
// gql`
//   fragment invalidUnionWithinUnion on CatOrDog {
//     ...humanOrAlienFragment # error
//   }
//   fragment humanOrAlienFragment on HumanOrAlien { __typename }
// `;

// interface into non implementing object
// gql`
//   fragment invalidInterfaceWithinObject on Cat {
//     ...intelligentFragment # error
//   }
//   fragment intelligentFragment on Intelligent { iq }
// `;

// interface into non overlapping interface
// gql`
//   fragment invalidInterfaceWithinInterface on Pet {
//     ...intelligentFragment # error
//   }
//   fragment intelligentFragment on Intelligent { iq }
// `;

// interface into non overlapping interface in inline fragment
gql`
  fragment invalidInterfaceWithinInterfaceAnon on Pet {
    ...on Intelligent { iq } # error
  }
`;

// interface into non overlapping union
// gql`
//   fragment invalidInterfaceWithinUnion on HumanOrAlien {
//     ...petFragment # error
//   }
//   fragment petFragment on Pet { name }
// `;
