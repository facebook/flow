/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {Comment, ESNode, Program} from 'flow-estree-oxidized';
import type {DetachedNode, MaybeDetachedNode} from '../detachedNodeTypes';

// $FlowExpectedError[untyped-import]
import {attach as untypedAttach} from './prettier/main/comments';
// $FlowExpectedError[untyped-import]
import {locEnd, locStart} from './prettier/language-js/loc';
// $FlowExpectedError[untyped-import]
import printer from './prettier/language-js/printer-estree';
import {
  // $FlowExpectedError[untyped-import]
  addLeadingComment as untypedAddLeadingComment,
  // $FlowExpectedError[untyped-import]
  addTrailingComment as untypedAddTrailingComment,
  // $FlowFixMe[untyped-import]
} from './prettier/common/util';
import {isBlockComment} from 'flow-estree-oxidized';
import {EOL} from 'os';

export type Options = $ReadOnly<{}>;

export enum CommentPlacement {
  LEADING_OWN_LINE,
  LEADING_INLINE,
  TRAILING_OWN_LINE,
  TRAILING_INLINE,
}

export function attachComments(
  comments: $ReadOnlyArray<Comment>,
  ast: Program,
  text: string,
): void {
  untypedAttach(comments, ast, text, {
    locStart,
    locEnd,
    printer,
  });
}

export function mutateESTreeASTCommentsForPrettier(
  program: Program,
  text: string,
): string {
  let code = text;

  // we need to delete the comments prop or else prettier will do
  // its own attachment pass after the mutation and duplicate the
  // comments on each node, borking the output
  // $FlowExpectedError[cannot-write]
  delete program.comments;

  // The docblock comment is never attached to any AST nodes, since its technically
  // attached to the program. However this is specific to our AST and in order for
  // prettier to correctly print it we need to attach it to the first node in the
  // program body.
  if (program.docblock != null && program.docblock.comment != null) {
    const docblockComment = program.docblock.comment;

    const isDocblockCommentNew = !isAttachedComment(docblockComment);
    if (isDocblockCommentNew) {
      // $FlowExpectedError[prop-missing]
      docblockComment.printed = false;
      // $FlowExpectedError[prop-missing]
      docblockComment.leading = true;
      // $FlowExpectedError[prop-missing]
      docblockComment.trailing = false;
    }

    // If we have a first node in the program body, attach the comment to that
    // otherwise set it on the program.
    if (program.body.length > 0) {
      const firstStatement = program.body[0];
      const leadingComments = getLeadingCommentsForNode(firstStatement);
      if (!leadingComments.includes(docblockComment)) {
        setCommentsOnNode(firstStatement, [
          docblockComment,
          ...getCommentsForNode(firstStatement),
        ]);

        if (isDocblockCommentNew) {
          // Add markers to the code block to ensure docblock comment is printed on its
          // own line.
          code = makeCommentOwnLine(code, docblockComment);
        }
      }
    } else {
      setCommentsOnNode(program, [docblockComment]);
    }
  }

  // Should not be needed anymore
  // $FlowExpectedError[cannot-write]
  delete program.docblock;

  return code;
}

export function moveCommentsToNewNode(
  oldNode: ESNode,
  newNode: DetachedNode<ESNode>,
): void {
  setCommentsOnNode(newNode, getCommentsForNode(oldNode));
  setCommentsOnNode(oldNode, []);
}

export function cloneCommentsToNewNode(
  oldNode: MaybeDetachedNode<ESNode>,
  newNode: MaybeDetachedNode<ESNode>,
): void {
  setCommentsOnNode(
    newNode,
    getCommentsForNode(oldNode).map(comment =>
      cloneCommentWithMarkers(comment),
    ),
  );
}

export function cloneJSDocCommentsToNewNode(
  oldNode: ESNode,
  newNode: MaybeDetachedNode<ESNode>,
): void {
  const comments = getCommentsForNode(oldNode).filter(comment => {
    return (
      isBlockComment(comment) &&
      // JSDoc comments always start with an extra asterisk
      comment.value.startsWith('*')
    );
  });
  setCommentsOnNode(newNode, [
    ...getCommentsForNode(newNode),
    ...comments.map(cloneCommentWithMarkers),
  ]);
}

export function setCommentsOnNode(
  node: ESNode | DetachedNode<ESNode>,
  comments: $ReadOnlyArray<Comment>,
): void {
  // $FlowExpectedError[prop-missing] - this property is secretly added by prettier.
  // $FlowExpectedError[incompatible-use] - this property is secretly added by prettier.
  // $FlowExpectedError[cannot-write] - this property is secretly added by prettier.
  node.comments = comments;
}

export function getCommentsForNode(
  node: ESNode | DetachedNode<ESNode>,
): $ReadOnlyArray<Comment> {
  // $FlowExpectedError[prop-missing] - this property is secretly added by prettier.
  // $FlowExpectedError[incompatible-use] - this property is secretly added by prettier.
  return node.comments ?? [];
}

export function isAttachedComment(comment: Comment): boolean {
  // Prettier adds this property to comments that have been attached.
  // $FlowExpectedError[prop-missing] - this property is secretly added by prettier.
  return comment.printed === false;
}

export function isLeadingComment(comment: Comment): boolean {
  // $FlowExpectedError[prop-missing] - this property is secretly added by prettier.
  return comment.leading === true;
}
export function isTrailingComment(comment: Comment): boolean {
  // $FlowExpectedError[prop-missing] - this property is secretly added by prettier.
  return comment.trailing === true;
}

export function getLeadingCommentsForNode(
  node: ESNode | DetachedNode<ESNode>,
): $ReadOnlyArray<Comment> {
  return getCommentsForNode(node).filter(isLeadingComment);
}

export function getTrailingCommentsForNode(
  node: ESNode | DetachedNode<ESNode>,
): $ReadOnlyArray<Comment> {
  return getCommentsForNode(node).filter(isTrailingComment);
}

export function addComment(
  node: ESNode | DetachedNode<ESNode>,
  comment: Comment,
  placement: CommentPlacement,
): void {
  switch (placement) {
    case CommentPlacement.LEADING_OWN_LINE:
    case CommentPlacement.LEADING_INLINE: {
      untypedAddLeadingComment(node, comment);
      break;
    }
    case CommentPlacement.TRAILING_OWN_LINE:
    case CommentPlacement.TRAILING_INLINE: {
      untypedAddTrailingComment(node, comment);
      break;
    }
  }
}

export function cloneComment<T: Comment>(comment: T): T {
  // $FlowExpectedError[incompatible-type]
  return {
    type: comment.type,
    value: comment.value,
    loc: comment.loc,
    range: comment.range,
  };
}

export function cloneCommentWithMarkers<T: Comment>(comment: T): T {
  // $FlowExpectedError[incompatible-type]
  return {
    type: comment.type,
    value: comment.value,
    loc: comment.loc,
    range: comment.range,
    leading: isLeadingComment(comment),
    trailing: isTrailingComment(comment),
  };
}

function getFirstNewlineIndex(code: string): number {
  return code.search(/\r\n|\n|\r/);
}

function getFirstNonWhitespaceIndex(code: string): number {
  return code.search(/\S/);
}

export function makeCommentOwnLine(code: string, comment: Comment): string {
  let newCode = code;
  // Since we always want a line break we need to ensure a newline is found when
  // searching out from either side of the comment range.
  let firstNewline = getFirstNewlineIndex(code);
  if (firstNewline === -1) {
    // No newline in file, lets add one.
    newCode += EOL;
    firstNewline = newCode.length;
  }

  // Prettier only uses these ranges for detecting whitespace, so this nonsensical
  // range is safe.
  // $FlowExpectedError[cannot-write]
  comment.range = [firstNewline + 1, firstNewline];

  return newCode;
}

export function appendCommentToSource(
  code: string,
  comment: Comment,
  placement: CommentPlacement,
): string {
  let newCode = code;
  switch (comment.type) {
    case 'Block': {
      // Prettier decides if a newline is necessary between the comment and its node by looking
      // to see if a newline seperates them in the source text. We can trick prettier into
      // formatting how we want for new comments by placing the range such that a newline
      // will (OWN_LINE) or will not (INLINE) be found when searching from the specified range
      // position.
      switch (placement) {
        case CommentPlacement.LEADING_OWN_LINE:
        case CommentPlacement.TRAILING_OWN_LINE: {
          newCode = makeCommentOwnLine(code, comment);
          break;
        }
        case CommentPlacement.LEADING_INLINE:
        case CommentPlacement.TRAILING_INLINE: {
          // Since we don't want a line break we need to ensure a non whitespace char is
          // always found before a newline when searching out from either side of the
          // comment range.
          let firstNonWhitespace = getFirstNonWhitespaceIndex(code);
          if (firstNonWhitespace === -1) {
            // No non whitespace chars in file, lets add an identifiable statement for prettier to find.
            newCode += '$FORCE_INLINE_ON_EMPTY_FILE_TOKEN$;';
            firstNonWhitespace = newCode.length;
            break;
          }

          // $FlowExpectedError[cannot-write]
          comment.range = [firstNonWhitespace + 1, firstNonWhitespace];
          break;
        }
      }
      break;
    }
    case 'Line': {
      // For `Line` comments prettier slices comments directly from the source code when printing
      // https://github.com/prettier/prettier/blob/5f0ee39fa03532c85bd1c35291450fe7ac3667b3/src/language-js/print/comment.js#L15-L20
      // this means that we need to have any appended comments directly in the
      // source code or else prettier will slice nothing and bork up the transform
      const commentText = `//${comment.value}`;

      const lastChar = newCode[newCode.length - 1];
      if (lastChar !== '\n' && lastChar !== '\r') {
        newCode += EOL;
      }

      // Line comments cannot be inline before a node so we only place trailing Line comments inline.
      if (placement === CommentPlacement.TRAILING_INLINE) {
        // Prettier determines an "end of line" comment by walking backwards from
        // the comment start range through the source code to see if it finds a non
        // newline token. In order to trick prettier for new comments we need to
        // insert fake source code for it to find.
        newCode += '$FORCE_END_OF_LINE_COMMENT_TOKEN$;';
      }
      const start = newCode.length;
      newCode += commentText;
      const end = newCode.length;

      // $FlowExpectedError[cannot-write]
      comment.range = [start, end];

      break;
    }
  }

  return newCode;
}
