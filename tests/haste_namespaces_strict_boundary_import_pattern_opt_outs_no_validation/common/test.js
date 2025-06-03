import gql from 'a.graphql'; // no conformance error, because validation is turned off
import params from 'b$Parameters'; // no missing in native error, because validation is turned off

// The web version will be picked
gql as string; // ok
gql as number; // error: string ~> number
params as string; // ok
params as number; // error: string ~> number
