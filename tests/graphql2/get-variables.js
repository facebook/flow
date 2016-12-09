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
