// @flow

const likeStory = gql`mutation {likeStory}`;
({storyID: '', inc: 1}: $GraphqlVars<typeof likeStory>);
({storyID: 123, inc: 1}: $GraphqlVars<typeof likeStory>); // number ~> string
({storyID: '123'}: $GraphqlVars<typeof likeStory>); // prop `inc` not found

const postMsg = gql`mutation {postMessage}`;
// list type
({msg: {text: '', tags: ['']}}: $GraphqlVars<typeof postMsg>);
// nullable
({msg: {text: '', tags: null}}: $GraphqlVars<typeof postMsg>);
// missing property in nested object
({msg: {text: 'hi'}}: $GraphqlVars<typeof postMsg>); // prop `tags` not found

gql`mutation {bad}`; // undefined mutation

const query = gql`
  query($id: ID!, $msg: MessageInput) { __typename }
`;
declare var queryInfered: $GraphqlVars<typeof query>;
declare var queryDeclared: {|
  id: string,
  msg: ?{text: string, tags: ?Array<string>},
|};
(queryDeclared: typeof queryInfered);
(queryInfered: typeof queryDeclared);
(queryInfered.id: number); // string ~> number
