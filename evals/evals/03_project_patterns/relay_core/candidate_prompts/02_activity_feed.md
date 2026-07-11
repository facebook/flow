You're building the home-page activity feed for an internal tool.

A Relay query has already been run on the server, and a reference to its result will be passed into your component. The result contains a feed: a list of activity entries. The entries are not all the same shape — each entry is one of several kinds, and each kind is displayed differently.

Write `ActivityFeed.react.js` that renders the feed as a list. For each entry:

- A **post** shows the author's name and a preview of what they posted. Posts also have a visibility level. Public and friends-only posts show the preview; private posts show only that the author shared a private post (no preview). When the component is in compact mode, even public and friends-only posts omit the preview.
- A **friend request** shows who sent it and how many mutual friends you share (pluralize the word "friend" correctly for 1 vs. many). The request may include a group you both belong to — if so, name it on the same line.
- An **event reminder** shows the event's name and the date it starts.
- **Likes and comments** on your posts are both social notifications — display them the same way.
- Any other kind of entry shows a generic "New activity" line.

Your component must:

- Use `@flow strict-local`.
- Accept the preloaded query reference as a prop.
- Support **compact mode**, a **maximum item count**, and a **timestamp visibility toggle** as display options. If a maximum is set, show only the first N entries; the toggle controls whether timestamps appear on entries that have them. Define any supporting types yourself — if a type makes sense as a separate reusable concern, put it in its own file and compose it into your component's props.
- Type-check with **zero errors** — run `flow` to confirm.

The query's generated Flow types are already present in the project. Read them to find the exact field names and the kinds of entries the feed can contain.
