// @flow

import { type CommitTemplate, wrapCommitTemplateWithState } from "./misplaced_error_exports";

const EMPTY_TEMPLATE: CommitTemplate<{...}> = {
  title: '',
  summary: '',
};

wrapCommitTemplateWithState(
  {...EMPTY_TEMPLATE, title: ({a}) => `${a}`}, // missing-local-annot due to inexact ~> exact
  {a: 'A'}
);
