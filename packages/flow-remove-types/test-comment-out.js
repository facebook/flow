#!/usr/bin/env node

const { exec } = require('node:child_process');
const fs = require('node:fs').promises;
const path = require('node:path');
const { promisify } = require('node:util');
// NOTE: chalk is dynamically imported because it is an ESM package
let chalk;
const diff = require('diff');

const FLOW_REMOVE_TYPES = 'flow-remove-types';

function readFile(filePath) {
  return fs.readFile(filePath, 'utf8');
}

function logGitDiffHeader(oldString, newString) {
  console.log(chalk.bold('--- a/original'));
  console.log(chalk.bold('+++ b/modified'));
  console.log(''); // Empty line for readability
}

function compare1(oldString, newString) {
  if (oldString === newString) {
    return true;
  }

  logGitDiffHeader();
  
  const changes = diff.diffChars(oldString, newString);
  
  const outputString = changes.reduce((s, change) => {
    const coloredText = change.added ? chalk.bgGreen.black(change.value) :
                        change.removed ? chalk.bgRed.white(change.value) :
                        change.value;
    return s + coloredText;
  }, '');
  
  console.log(outputString);
  
  return false;
}

function compare2(oldString, newString, options = {}) {
  if (oldString === newString) {
    return true;
  }

  const {
      showLineNumbers = true,
      ignoreWhitespace = false
  } = options;

  const generateLineNumber = (lineNumber) => showLineNumbers ? `${lineNumber.toString().padStart(4)} ` : '';

  logGitDiffHeader()

  // Choose diff method based on options
  const diffMethod = ignoreWhitespace ? diff.diffWordsWithSpace : diff.diffLines;
  const changes = diffMethod(oldString, newString);

  let output = [];
  let oldLineNumber = 1;
  let newLineNumber = 1;

  for (const change of changes) {
      const lines = change.value.split('\n');
      
      // Remove empty last line if it exists due to split
      if (lines.at(-1) === '') {
          lines.pop();
      }
      
      if (change.added) {
          // Added lines (green with + prefix)
          for (const line of lines) {
              const lineNumber = generateLineNumber(newLineNumber);
              output.push(chalk.green(`+${lineNumber}${line}`));
              newLineNumber++;
          }
      } else if (change.removed) {
          // Removed lines (red with - prefix)
          for (const line of lines) {
              const lineNumber = generateLineNumber(oldLineNumber);
              output.push(chalk.red(`-${lineNumber}${line}`));
              oldLineNumber++;
          }
      } else {
          // Unchanged lines (no color, space prefix)
          for (const line of lines) {
              const lineNumber = generateLineNumber(oldLineNumber);
              output.push(` ${lineNumber}${line}`);
              oldLineNumber++;
              newLineNumber++;
          }
      }
  }
  
  const outputString = output.join('\n');
  console.log(outputString);

  return false;
}

function compare3(oldString, newString) {
  if (oldString === newString) {
    return true;
  }

  logGitDiffHeader();
  
  const changes = diff.diffWords(oldString, newString);

  const outputString = changes.reduce((s, change) => {
    const coloredText = change.added ? chalk.green.bold(change.value) :
                        change.removed ? chalk.red.bold(change.value) :
                        change.value;
    return s + coloredText + '\n';
  }, '');
  console.log(outputString);

  return false;
}

function showUsage() {
  console.error('Usage: ./test-comment-out.js <source-file> <expected-file>');
  console.error('Example: ./test-comment-out.js test/source.js test/expected.js');
  process.exit(1);
}

async function runTest(sourceFilePath, expectedFilePath) {
  console.log(`Test: ${FLOW_REMOVE_TYPES} --comment-out ${sourceFilePath}`);

  try {
    chalk = (await import('chalk')).default;

    const scriptPath = path.resolve(__dirname, FLOW_REMOVE_TYPES);

    const execAsync = promisify(exec);
    const { stdout: actual } = await execAsync(`${scriptPath} --comment-out ${sourceFilePath}`);

    const expected = await readFile(expectedFilePath);

    if (compare1(expected, actual)) {
      console.log('Test passed!');
      process.exit(0);
    } else {
      process.exit(1);
    }
  } catch (error) {
    console.error('Test failed with error:', error.message);
    process.exit(1);
  }
}

const args = process.argv.slice(2);
if (args.length !== 2) {
  showUsage();
}
const [sourceFilePath, expectedFilePath] = args;
if (!sourceFilePath || !expectedFilePath) {
  showUsage();
}
runTest(sourceFilePath, expectedFilePath).catch((error) => {
  console.error('Unhandled error:', error);
  process.exit(1);
});