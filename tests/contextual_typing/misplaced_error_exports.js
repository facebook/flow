// @flow

export type CommitTemplate<TCommitState> = {
  title: string | (TCommitState => string),
  summary: string | (TCommitState => string),
  tags?: ?$ReadOnlyArray<string> | (TCommitState => $ReadOnlyArray<string>),
};

export function wrapCommitTemplateWithState<
  TCommitStateEntry,
  TCommitStateResult,
>(
  commitTemplate: CommitTemplate<{|...TCommitStateResult, ...TCommitStateEntry|}>,
  commitState: TCommitStateEntry,
): CommitTemplate<TCommitStateResult> {throw ''}
