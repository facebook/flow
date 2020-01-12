// @flow

// I (nmote) am adding this test to ensure that a file with a parse error does not stymie our
// ability to skip direct dependent files in recheck opts. We have to check direct dependents when a
// file *becomes* unparsed, but I want to make sure that we aren't overly-conservative, and that we
// can still skip direct dependents when a file is unparsed and continues to be unparsed.

=
