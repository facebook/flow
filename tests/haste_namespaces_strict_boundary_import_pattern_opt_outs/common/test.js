import gql from 'a.graphql'; // error: conformance error
import params from 'b$Parameters'; // error: missing in native

// The web version will be picked
gql as string; // ok
gql as number; // error: string ~> number
params as string; // ok
params as number; // error: string ~> number
