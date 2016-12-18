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

/*
 * Data from unions
 */

const unionQuery = gql`
  fragment on StoryOrEvent {
    __typename
    ... on Story { text }
    ... on Event { date }
  }
`;
declare var union: $GraphqlData<typeof unionQuery>;

switch (union.__typename) {
  case 'Story':
    (union.text: string);
    union.date; // error: `date` is from `Event`
    break;
  case 'Event':
    (union.date: ?string);
    union.text; // error: `text` is from `Story`
    break;
  default:
    (union: empty); // ok: union is exhausted by here
}
