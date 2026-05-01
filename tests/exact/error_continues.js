declare var any: any;

any as {p: number} as {|p: string|}; // We should get two errors here!
                                     // One for the inexact ~> exact, and
                                     // one for number ~> string
