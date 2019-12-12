/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import {join} from 'path';
import {format} from 'util';

import * as blessed from 'blessed';

import {readFile, writeFile} from '../utils/async';
import {
  mainLocOfError,
  prettyPrintError,
  mergedMessagesOfError,
  prettyPrintMessageOfError,
} from '../flowResult';
import getPathToLoc from './getPathToLoc';
import getFlowErrors from './getFlowErrors';
import getContext, {NORMAL, JSX, JSX_FRAGMENT, TEMPLATE} from './getContext';
import getAst from './getAst';

import type {PathNode} from './getPathToLoc';
import type {Args} from './add-commentsCommand';
import type {FlowLoc, FlowError, FlowMessage} from '../flowResult';
import type {Context} from './getContext';

export type Suppression = {|
  loc: FlowLoc,
  isError: boolean,
  lints: Set<string>,
|};

const unselectedBox = '[ ]';
const selectedBox = '[\u2713]'; // checkmark
const someSelected = '[~]';

// Wraps a Flow error with a few other bits of info
class BlessedError {
  error: FlowError;
  pretty: ?string = null;
  selected: string;
  active: boolean;
  selectedMessage: ?number = null;
  messages: Array<FlowMessage>;

  constructor(error: FlowError) {
    this.error = error;
    this.selected = unselectedBox;
    this.active = true;
    this.messages = mergedMessagesOfError(this.error);
  }

  prettyPrint(): string {
    if (this.pretty == null) {
      const selectedIndex = this.selectedMessage || 0;
      const messages = this.messages.map((m, idx) => {
        const pp = prettyPrintMessageOfError(this.error, m);
        return idx === selectedIndex
          ? format('{white-fg}{blue-bg}%s{/}', pp)
          : pp;
      });
      this.pretty = messages.join('\n');
    }
    return this.pretty;
  }

  getLocation(): ?FlowLoc {
    if (this.selectedMessage != null) {
      return this.messages[this.selectedMessage].loc;
    } else {
      return mainSourceLocOfError(this.error);
    }
  }

  getStringOfLocation(): string {
    const loc = this.getLocation();
    if (loc == null) {
      return '[No location]';
    }
    return format('%s:%s', loc.source || '[No file]', loc.start.line);
  }

  setSelectedMessage(index: number): void {
    this.selectedMessage = index;
    this.pretty = null;
  }

  setSelectedMessageWithRegex(regex: RegExp): void {
    let selectedMessage = null;
    let lastMessageWithLoc = null;
    for (let idx = 0; idx < this.messages.length; idx++) {
      const message = this.messages[idx];
      if (message.loc != null) {
        lastMessageWithLoc = idx;
      }
      if (prettyPrintMessageOfError(this.error, message).match(regex) != null) {
        if (lastMessageWithLoc != null) {
          selectedMessage = lastMessageWithLoc;
        }
        break;
      }
    }
    selectedMessage && this.setSelectedMessage(selectedMessage);
  }

  setActive(activeRegex) {
    this.active =
      blessed.stripTags(this.prettyPrint()).match(activeRegex) != null;
  }

  select() {
    this.selected = selectedBox;
  }

  unselect() {
    this.selected = unselectedBox;
  }

  toggleSelect() {
    if (this.selected === unselectedBox) {
      this.select();
    } else {
      this.unselect();
    }
  }

  isSelected() {
    return this.selected === selectedBox;
  }
}

function mainSourceLocOfError(error: FlowError): ?FlowLoc {
  const {operation, message} = error;
  for (const msg of [operation, ...message]) {
    if (msg && msg.loc && msg.loc.type === 'SourceFile') {
      return msg.loc;
    }
  }
  return null;
}

/**
 * Filter out errors without a main location or a source file
 */
function filterErrors(errors: Array<FlowError>): Array<FlowError> {
  return errors.filter(e => mainSourceLocOfError(e) != null);
}

/**
 * Wrap errors with some extra functionality
 */
function blessErrors(errors: Array<FlowError>): Array<BlessedError> {
  return errors.map(e => new BlessedError(e));
}

async function nonInteractive(args: Args): Promise<void> {
  let flowResult = await getFlowErrors(
    args.bin,
    args.errorCheckCommand,
    args.root,
    args.flowconfigName,
  );

  if (flowResult.passed) {
    console.log('No errors found. Nothing to do. Exiting');
    return;
  }

  const errors = blessErrors(filterErrors(flowResult.errors));

  await addComments(args, errors);
  process.exit(0);
}

async function interactive(args: Args): Promise<void> {
  // Use blessed to select which comments to remove

  // Create a screen object.
  const screen = blessed.screen({
    autoPadding: true,
    smartCSR: true,
    title: 'foo',
  });

  // Quit on Escape, q, or Control-C.
  screen.key(['escape', 'q', 'C-c'], () => {
    screen.destroy();
    process.exit(0);
  });

  // A containing box
  const outer = blessed.box({
    parent: screen,
    width: '100%',
    height: '100%',
    style: {
      bg: 'white',
      fg: 'blue',
    },
  });

  // A message to show while we query flow for the errors
  const loading = blessed.loading({
    parent: outer,
    left: 'center',
    top: 'center',
    border: 'line',
    align: 'center',
  });
  loading.load(format('Running `%s check %s`', args.bin, args.root));
  // Lading usually disables all keys, but we still want to be able to quit
  screen.lockKeys = false;
  screen.render();
  let flowResult;
  try {
    flowResult = await getFlowErrors(
      args.bin,
      args.errorCheckCommand,
      args.root,
      args.flowconfigName,
    );
  } catch (e) {
    screen.destroy();
    throw e;
  }
  loading.stop();

  if (flowResult.passed) {
    screen.destroy();
    console.log('No errors found. Nothing to do. Exiting');
    return;
  }

  // A list of file locations
  var locations = blessed.listtable({
    parent: outer,
    label: blessed.parseTags('{grey-fg}{white-bg}Locations{/}'),
    align: 'left',
    width: '30%',
    height: '100%',
    style: {
      fg: 'grey',
      bg: 'white',
      header: {
        fg: 'grey',
        bg: 'white',
        bold: true,
        underline: true,
      },
      cell: {
        fg: 'grey',
        bg: 'white',
        selected: {
          fg: 'white',
          bg: 'blue',
        },
      },
      border: {
        fg: 'black',
        bg: 'white',
      },
    },
    border: 'line',
    // Allow mouse support
    mouse: true,
    // Allow key support (arrow keys + enter)
    keys: true,
    // Use vi built-in keys
    vi: true,
  });

  // Filter out errors without a main location
  const errors = blessErrors(filterErrors(flowResult.errors));

  let locationToErrorsMap: Map<string, Array<BlessedError>>;
  let scrollToLocationMap;
  let sort: 'count' | 'loc' = 'count';

  // Shows the individual errors for a particular location
  const details = blessed.box({
    parent: outer,
    border: 'line',
    right: 0,
    bottom: 3,
    width: '70%',
    height: 'shrink',
    content: 'details go here',
    align: 'left',
    scrollable: true,
    alwaysScroll: true,
    // Allow mouse support
    mouse: true,
    // Allow key support (arrow keys + enter)
    keys: true,
    // Use vi built-in keys
    vi: true,
    style: {
      bg: 'white',
      fg: 'black',
      border: {
        fg: 'black',
        bg: 'white',
      },
    },

    scrollbar: {
      bg: 'green',
      ch: ' ',
      track: {
        bg: 'blue',
      },
    },
  });

  // Shows the active filter
  const filterText = blessed.text({
    parent: outer,
    align: 'center',
    bottom: 0,
    right: 0,
    height: 'shrink',
    width: 'shrink',
    content: 'No active filter',
    style: {
      bg: 'white',
      fg: 'red',
    },
  });
  const locRegexText = blessed.text({
    parent: outer,
    align: 'center',
    bottom: 1,
    right: 0,
    height: 'shrink',
    width: 'shrink',
    content: 'No location regex',
    style: {
      bg: 'white',
      fg: 'red',
    },
  });
  var numberActiveText = blessed.text({
    parent: outer,
    align: 'center',
    bottom: 2,
    right: 0,
    height: 'shrink',
    width: 'shrink',
    content: '0 errors selected',
    style: {
      bg: 'white',
      fg: 'red',
    },
  });

  // A key to show which keys do what
  const keyText = blessed.text({
    parent: outer,
    align: 'left',
    bottom: 0,
    left: '30%',
    height: 2,
    width: 'shrink',
    content:
      '(enter/space) select; (a) toggle all; (c) add comments;\n' +
      '(g) auto-group; (s) toggle sort; (/) filter; (l) loc regex; ( \u21FD / \u21FE ) focus',
    style: {
      bg: 'white',
      fg: 'red',
    },
  });

  const renderLocations = () => {
    locationToErrorsMap = new Map();
    scrollToLocationMap = new Map();
    for (const error of errors) {
      if (error.active) {
        const locString = error.getStringOfLocation();
        const errorsOfLoc = locationToErrorsMap.get(locString) || [];
        errorsOfLoc.push(error);
        locationToErrorsMap.set(locString, errorsOfLoc);
      }
    }
    const rows = [['Selected', 'Location']];
    const entries = Array.from(locationToErrorsMap.entries());
    entries.sort(([aLocString, aErrorsOfLoc], [bLocString, bErrorsOfLoc]) => {
      switch (sort) {
        case 'loc':
          return aLocString.localeCompare(bLocString);
        case 'count':
          return bErrorsOfLoc.length - aErrorsOfLoc.length;
        default:
          throw new Error('Unexpected sort type: ' + sort);
      }
    });
    entries.forEach(([locString, errorsOfLoc], scrollIndex) => {
      scrollToLocationMap.set(scrollIndex + 1, locString);
      const numSelected = errorsOfLoc.filter(e => e.selected === selectedBox)
        .length;
      const selected =
        numSelected === 0
          ? unselectedBox
          : numSelected === errorsOfLoc.length
          ? selectedBox
          : someSelected;
      rows.push([selected, format('%s (%d)', locString, errorsOfLoc.length)]);
    });

    numberActiveText.setContent(
      format('%d errors selected', errors.filter(e => e.isSelected()).length),
    );

    locations.setRows(rows);
  };
  renderLocations();

  // We we scroll the locations update the details
  const showDetails = scroll => {
    const location = scrollToLocationMap.get(scroll);
    if (location != undefined) {
      const errors = locationToErrorsMap.get(location);

      if (errors !== undefined) {
        let content = blessed.parseTags(
          errors.map(e => e.prettyPrint()).join('\n\n'),
        );
        details.setContent(content);
        screen.render();
      }
    }
  };

  locations.on('scroll', () => showDetails(locations.getScroll()));

  // When we select a location, select all its active errors
  locations.on('select', (_, selected) => {
    const location = scrollToLocationMap.get(selected);
    if (location != undefined) {
      const errors = locationToErrorsMap.get(location);

      if (errors != undefined) {
        errors.forEach(error => error.toggleSelect());
        renderLocations();
        locations.select(selected);
      }
    }
  });

  // Space also should select
  locations.key('space', () => {
    locations.enterSelected();
  });

  // Use arrow keys to shift focus (clicking also should work)
  details.key('left', () => {
    locations.focus();
  });
  locations.key('right', () => {
    details.focus();
  });

  // Selects all the active errors
  screen.key('a', key => {
    const selected = locations.selected;
    const activeErrors = errors.filter(e => e.active);
    const allSelected = activeErrors.every(e => e.isSelected());
    activeErrors.forEach(e => {
      allSelected ? e.unselect() : e.select();
    });
    renderLocations();
    locations.select(selected);
  });

  // Blue shows what is active
  [details, locations].forEach(elem => {
    elem.on('focus', () => {
      elem.style.border.fg = 'blue';
      (elem.style.cell ? elem.style.cell : elem.style).fg = 'blue';
      screen.render();
    });
    elem.on('blur', () => {
      elem.style.border.fg = 'black';
      (elem.style.cell ? elem.style.cell : elem.style).fg = 'black';
      screen.render();
    });
  });

  // `/` opens up a prompt for a regex with which to filter the errors
  const search = blessed.prompt({
    parent: outer,
    border: 'line',
    height: 'shrink',
    width: 'half',
    top: 'center',
    left: 'center',
    label: ' {blue-fg}Filter{/blue-fg} ',
    tags: true,
    keys: true,
    vi: true,
  });

  let lastSearch = '';
  screen.key('/', () =>
    search.input('Enter search regex', lastSearch, (err, value) => {
      if (err == null && value != null) {
        try {
          // We don't know if value is a valid regex
          const regex = new RegExp(value);
          filterText.setContent(
            value === ''
              ? 'No active filter'
              : format('Active filter: %s', regex),
          );
          lastSearch = value;
          errors.forEach(e => e.setActive(regex));
          renderLocations();
        } catch (e) {
          screen.render();
          return;
        }
      }
    }),
  );

  // `l` opens up a prompt for a regex
  const locSearch = blessed.prompt({
    parent: outer,
    border: 'line',
    height: 'shrink',
    width: 'half',
    top: 'center',
    left: 'center',
    label: ' {blue-fg}Location regex{/blue-fg} ',
    tags: true,
    keys: true,
    vi: true,
  });

  let lastLocSearch = '';
  screen.key('l', () =>
    search.input('Enter location regex', lastLocSearch, (err, value) => {
      if (err == null && value != null) {
        try {
          // We don't know if value is a valid regex
          const regex = new RegExp(value);
          locRegexText.setContent(
            value === ''
              ? 'No location regex'
              : format('Active regex: %s', regex),
          );
          lastLocSearch = value;
          errors.forEach(e => e.setSelectedMessageWithRegex(regex));
          renderLocations();
        } catch (e) {
          screen.render();
          return;
        }
      }
    }),
  );

  screen.key('g', () => {
    groupErrors(errors.filter(e => e.active));
    renderLocations();
  });

  screen.key('s', () => {
    sort = sort === 'loc' ? 'count' : 'loc';
    renderLocations();
  });

  const enterCommentPrompt = blessed.prompt({
    parent: outer,
    border: 'line',
    height: 'shrink',
    width: 'half',
    top: 'center',
    left: 'center',
    label: ' {blue-fg}Enter Comment{/blue-fg} ',
    tags: true,
    keys: true,
    vi: true,
  });

  const doAddComments = async () => {
    const selectedErrors = errors.filter(e => e.isSelected());
    if (args.comment == null) {
      enterCommentPrompt.input('Enter comment', async (err, value) => {
        if (err == null && value != null) {
          args.comment = value;
          screen.destroy();
          await addComments(args, selectedErrors);
          process.exit(0);
        }
      });
    } else {
      screen.destroy();
      await addComments(args, selectedErrors);
      process.exit(0);
    }
  };

  screen.key('c', doAddComments);

  showDetails(1);
  locations.focus();
  screen.render();
}

export default async function(args: Args): Promise<void> {
  if (args.all) {
    await nonInteractive(args);
  } else {
    await interactive(args);
  }
}

function groupErrors(errors: Array<BlessedError>): void {
  let ungroupedErrors = errors;
  // Loop through the active errors, choose the most popular location, and
  // select that location. Then loop again with the still unchosen errors
  while (ungroupedErrors.length > 0) {
    // For each error, figure out which locations it refers to
    const possibleLocsPerError = ungroupedErrors.map(error => {
      const locToFirstIndexMap = new Map();
      error.messages.forEach((message, msgIdx) => {
        if (
          message.loc &&
          message.loc.source &&
          message.loc.type == 'SourceFile'
        ) {
          const locString = format(
            '%s:%s',
            message.loc.source,
            message.loc.start.line,
          );
          if (!locToFirstIndexMap.has(locString)) {
            locToFirstIndexMap.set(locString, msgIdx);
          }
        }
      });
      return locToFirstIndexMap;
    });

    // Find the most popular location
    const locToErrorsMap = new Map();
    let mostPopularErrors = [];
    possibleLocsPerError.forEach((locToFirstIndexMap, errorIdx) => {
      locToFirstIndexMap.forEach((msgIdx, locString) => {
        const errors = locToErrorsMap.get(locString) || [];
        errors.push([errorIdx, msgIdx]);
        locToErrorsMap.set(locString, errors);

        if (errors.length > mostPopularErrors.length) {
          mostPopularErrors = errors;
        }
      });
    });

    // Select the popular location for the relevant errors
    const groupedErrorIndexes = new Set();
    for (const [errorIdx, msgIdx] of mostPopularErrors) {
      groupedErrorIndexes.add(errorIdx);
      ungroupedErrors[errorIdx].setSelectedMessage(msgIdx);
    }

    // Forget about the errors that we just selected
    let stillUngroupedErrors = [];
    for (let i = 0; i < ungroupedErrors.length; i++) {
      if (!groupedErrorIndexes.has(i)) {
        stillUngroupedErrors.push(ungroupedErrors[i]);
      }
    }
    ungroupedErrors = stillUngroupedErrors;
  }
}

async function addComments(args: Args, errors: Array<BlessedError>) {
  const seen = new Set();
  let filenameToLineToLocsMap: Map<
    string,
    Map<number, Suppression>,
  > = new Map();
  // Filter out errors without a main location, and dedupe errors that share
  // the same main location
  let errorCount = 0;
  for (const error of errors) {
    const loc = error.getLocation();
    if (loc != null && loc.source != null) {
      const source = join(args.root, loc.source);
      const lineToLocsMap = filenameToLineToLocsMap.get(source) || new Map();
      const prevValue = lineToLocsMap.get(loc.start.line);
      const isError =
        (prevValue && prevValue.isError) || error.error.kind !== 'lint';
      let lints = (prevValue && prevValue.lints) || new Set();
      if (error.error.kind === 'lint') {
        // \u0060 is `. using the escape to avoid a syntax highlighting bug in vscode-language-babel
        const match = /\(\u0060([^\u0060]+)\u0060\)$/.exec(
          error.error.message[0].descr,
        );
        if (match) {
          lints.add(match[1]);
        }
      }
      const value = {loc, isError, lints};
      lineToLocsMap.set(loc.start.line, value);
      filenameToLineToLocsMap.set(source, lineToLocsMap);
      errorCount++;
    }
  }

  const promises = [];
  for (const [source, lineToLocsMap] of filenameToLineToLocsMap.entries()) {
    promises.push(
      addCommentsToSource(args, source, Array.from(lineToLocsMap.values())),
    );
  }
  const counts = await Promise.all(promises);
  const commentCount = counts.reduce((c1, c2) => c1 + c2, 0);
  console.log(
    'Added %d comments to suppress %d errors',
    commentCount,
    errorCount,
  );
}

/* A single file needs 1 or more comments added. Start at the bottom of the
 * file, and add comments going up. Then write the changes */
async function addCommentsToSource(
  args: Args,
  source: string,
  locs: Array<Suppression>,
): Promise<number> {
  const codeBuffer = await readFile(source);

  const [code, commentCount] = await addCommentsToCode(
    args.comment,
    codeBuffer.toString(),
    locs,
    args.bin,
  );
  await writeFile(source, code);
  return commentCount;
}

export async function addCommentsToCode(
  comment: ?string,
  code: string,
  locs: Array<Suppression>,
  flowBinPath: string,
): Promise<
  [string, number],
> /* [resulting code, number of comments inserted] */ {
  locs.sort((l1, l2) => l2.loc.start.line - l1.loc.start.line);

  const ast = await getAst(code, flowBinPath);

  let commentCount = 0;
  for (const {loc, isError, lints} of locs) {
    const path = getPathToLoc(loc, ast);

    if (path != null) {
      let c = comment || '';
      let wrap = true;
      if (!isError && lints.size > 0) {
        const sortedLints = Array.from(lints).sort();
        c = `flowlint-next-line ${sortedLints
          .map(lint => `${lint}:off`)
          .join(', ')}`;
        wrap = false;
      }
      code = addCommentToCode(c, code, loc, path, wrap);
      commentCount++;
    }
  }
  return [code, commentCount];
}

function addCommentToCode(
  comment: string,
  code: string,
  loc: FlowLoc,
  path: Array<PathNode>,
  wrap: boolean,
): string {
  /* First of all, we want to know if we can add a comment to the line before
   * the error. So we need to see if we're in a JSX children block or inside a
   * template string when we reach the line with the error */
  const [inside, ast] = getContext(loc, path);

  const lines = code.split('\n');
  if (inside === NORMAL) {
    // This is easy, just add the comment to the preceding line
    return []
      .concat(
        lines.slice(0, loc.start.line - 1),
        formatComment(comment, lines[loc.start.line - 1], {wrap}),
        lines.slice(loc.start.line - 1),
      )
      .join('\n');
  } else if (
    (inside === JSX_FRAGMENT || inside === JSX) &&
    ast.type === 'JSXElement'
  ) {
    /* Ok, so we have something like
     * <jsx>
     *   <foo id={10*'hello'} />
     * <jsx>
     *
     * We need to put an empty expression container above the element with our
     * comment.
     *
     * <jsx>
     *   {// Comment
     *   }
     *   <foo id={10*'hello'} />
     * <jsx>
     */
    return []
      .concat(
        lines.slice(0, loc.start.line - 1),
        formatComment(comment, lines[loc.start.line - 1], {jsx: true, wrap}),
        lines.slice(loc.start.line - 1),
      )
      .join('\n');
  } else if (
    inside === TEMPLATE ||
    (inside === JSX && ast.type === 'JSXExpressionContainer')
  ) {
    /* Ok, so we have something like
     *
     * <jsx>
     *   {10 * 'hello'}
     * <jsx>
     *
     * We need to stick the comment inside the expression container.
     * So the above example turns into
     *
     * <jsx>
     *   {
     *     // Comment
     *     10 * 'hello'}
     * <jsx>
     *
     * Same thing if we have something like
     *
     * var str = `hello
     *   ${10 * 'hello'}
     * `;
     *
     * We need to stick the comment inside of the template element. So the
     * above example turns into
     *
     * var str = `hello
     *   ${
     *     // Comment
     *     10 * 'hello'}
     * `;
     */
    const start_col =
      inside === JSX ? ast.loc.start.column + 1 : ast.loc.start.column;
    const line = lines[loc.start.line - 1];
    const part1 = line.substr(0, start_col);
    const match = part1.match(/^ */);
    const padding = match ? match[0] + '  ' : '  ';
    const part2 = padding + line.substr(start_col);
    return []
      .concat(
        lines.slice(0, ast.loc.start.line - 1),
        [part1],
        formatComment(comment, part2, {wrap}),
        [part2],
        lines.slice(ast.loc.start.line),
      )
      .join('\n');
  }

  return code;
}

/* Take up to `max` characters from str, trying to split at a space or dash or
 * something like that. */
function splitAtWord(str: string, max: number): [string, string] {
  let ret = '';
  let maybe = '';

  for (let i = 0; i < max; i++) {
    if (i === str.length) {
      ret += maybe;
      break;
    }
    maybe += str[i];
    if (str[i].match(/[- _\t]/)) {
      ret += maybe;
      maybe = '';
    }
  }

  // If there were no breaks then take it all
  if (ret === '') {
    ret = maybe;
  }

  return [ret, str.substr(ret.length)];
}

/* Figures out how to pad the comment and split it into multiple lines */
function formatComment(
  comment: string,
  line: string,
  args: {|
    jsx?: boolean,
    wrap?: boolean,
  |},
): Array<string> {
  const {jsx = false, wrap = true} = args;
  const match = line.match(/^ */);
  let padding = match ? match[0] : '';
  padding.length > 40 && (padding = '    ');

  if (jsx === false) {
    const singleLineComment = format('%s// %s', padding, comment);
    if (!wrap || singleLineComment.length <= 80) {
      return [singleLineComment];
    }
  }

  const firstLinePrefix = format(!jsx ? '%s/* ' : '%s{/* ', padding);

  if (!wrap) {
    // jsx === true
    return [format('%s{/* %s */}', padding, comment.trim())];
  }

  const commentLines = [];
  let firstLineComment;
  [firstLineComment, comment] = splitAtWord(
    comment.trim(),
    80 - firstLinePrefix.length,
  );
  commentLines.push(firstLinePrefix + firstLineComment.trim());

  const prefix = format(!jsx ? '%s * ' : '%s  * ', padding);
  let commentLine;
  while (comment.length > 0) {
    [commentLine, comment] = splitAtWord(comment.trim(), 80 - prefix.length);
    commentLines.push(prefix + commentLine.trim());
  }
  if (commentLines[commentLines.length - 1].length < 76) {
    const last = commentLines.pop();
    commentLines.push(format(!jsx ? '%s */' : '%s */}', last));
  } else {
    commentLines.push(format(!jsx ? '%s */' : '%s  */}', padding));
  }
  return commentLines;
}
