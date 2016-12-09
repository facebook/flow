// @flow

const storyFrag = gql`
  fragment on Story {
    id
    tags
  }
`;
declare var story: $GraphqlData<typeof storyFrag>;
(story.id: string);
(story.tags: Array<string>); // `tags` is maybe type
story.text; // exists in schema but not fetched
story.bad; // field not found
