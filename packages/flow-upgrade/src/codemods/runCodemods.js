/**
 * @format
 * @flow
 */

const path = require('path');
const fs = require('fs');
const Runner = require('jscodeshift/src/Runner');

const AGGREGATE_CODEMOD_UTIL = path.join(
  __dirname,
  'createAggregateCodemod.js',
);

/**
 * Runs some codemods.
 *
 * We use the synchronous methods from `fs` for now as we fully expect for this
 * function to block the running process.
 */
module.exports = async function runCodemods(
  transformPaths: Array<string>,
  filePaths: Array<string>,
) {
  // Create a temporary for our aggregate codemod file.
  const aggregateTransformPath = path.join(
    fs.mkdtempSync('/tmp/flow-upgrade-'),
    'codemod.js',
  );
  // The contents of our transform file.
  const aggregateTransformContents = `
/**
 * This jscodeshift transform file was automatically generated by:
 * ${__filename}
 */
module.exports = require(${JSON.stringify(AGGREGATE_CODEMOD_UTIL)})([
${transformPaths
    .map(transformPath => `  ${JSON.stringify(transformPath)},`)
    .join('\n')}
]);
`.slice(1);
  // Write the codemod to the folder we created for it.
  fs.writeFileSync(aggregateTransformPath, aggregateTransformContents);
  // Run the codemod with jscodeshift! The runner returns a promise which we
  // will wait for.
  await Runner.run(aggregateTransformPath, filePaths, {
    parser: 'flow',
    verbose: 0,
  });
};
