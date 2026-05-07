export type CommitTemplate<TCommitState> = {
  title: string | (TCommitState => string),
  summary: string | (TCommitState => string),
  tags?: ?ReadonlyArray<string> | (TCommitState => ReadonlyArray<string>),
};

export function wrapCommitTemplateWithState<
  TCommitStateEntry,
  TCommitStateResult,
>(
  commitTemplate: CommitTemplate<{...TCommitStateResult, ...TCommitStateEntry}>,
  commitState: TCommitStateEntry,
): CommitTemplate<TCommitStateResult> {throw ''}
