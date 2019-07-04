/**
 * @format
 * @flow
 */

const semver = require('semver');
const chalk = require('chalk');
const ora = require('ora');
const Confirm = require('prompt-confirm');
const Styled = require('./Styled');
const findFlowFiles = require('./findFlowFiles');

import type {Upgrade} from './Types';

/**
 * Holds all of the upgrades that need to be run to upgrade to each version from
 * the last version of Flow.
 */
const VERSION_UPGRADES: Array<{|
  version: string,
  upgrades: Array<Upgrade>,
|}> = [
  // We started writing and distributing upgrades with `flow-upgrade` after Flow
  // version 0.52.0. We assume that you have valid Flow 0.52.0 code and are not
  // backporting any previous automated upgrades.

  {
    version: '0.53.0',
    upgrades: [
      require('./upgrades/0.53.0/ReactComponentExplicitTypeArgs'),
      require('./upgrades/0.53.0/ReactComponentSimplifyTypeArgs'),
      require('./upgrades/0.53.0/ReactUtilityTypes'),
    ],
  },
  {
    version: '0.84.0',
    upgrades: [require('./upgrades/0.84.0/ExplicitInexactObjectSyntax')],
  },
];

/**
 * Decides all of the upgrades that will need to be run when moving from one
 * version to another and runs them asynchronously.
 */
module.exports = async function upgrade(
  directory: string,
  currentVersion: string,
  nextVersion: string,
  options: {+all: boolean},
): Promise<void> {
  const allUpgrades = [];
  // Collect all of the upgrades we will need to run.
  for (let i = 0; i < VERSION_UPGRADES.length; i++) {
    const {version, upgrades} = VERSION_UPGRADES[i];
    // If the version number is larger then the version we are upgrading to we
    // are done! Do not add any more upgrades,
    if (semver.cmp(version, '>', nextVersion)) {
      break;
    }
    // If the version number is larget then the current version, but not larger
    // then the next version then we want to run the upgrades for this version.
    if (semver.cmp(version, '>', currentVersion)) {
      upgrades.forEach(upgrade => allUpgrades.push(upgrade));
    }
  }
  console.log();
  console.log(Styled.sectionHeader('flow-upgrade'));
  console.log();
  {
    const titleStyled = chalk.bold('flow-upgrade');
    // Log the initial intro message for our user.
    const message = `
Thank you for choosing to upgrade Flow. In every release the Flow community adds
new features and improves the performance of Flow to better serve you and your
team. Upgrading gives you the best that Flow has to offer.

Sometimes Flow will release breaking changes when they make sense or Flow has a
new best practice that your code can be upgraded to take advantage of. This
script will try its best to upgrade your codebase to the latest behavior and
best practices in the next version of Flow.

After running this script you may have new errors when you run Flow, or this
script may transform your code in a way that you do not want. Before running
${titleStyled}, make sure that you have a backup of your codebase. (If you are
using git make sure you've commit the changes in your working directory.) After
running ${titleStyled} review the changes that were made and manually tweak
your code until you are happy with the upgrade.

The Flow version you're upgrading from: ${chalk.bold.magenta(currentVersion)}
The Flow version you're upgrading to:   ${chalk.bold.green(nextVersion)}

We will assume that your code is valid Flow ${chalk.bold.magenta(
      currentVersion,
    )} code.

We will be running the following steps to upgrade your codebase. Each step will
print a short message explaining what it does before it runs, but we won't stop
to ask you for reconfirmation once we begin upgrading.

${allUpgrades.map((u, i) => Styled.upgradeTitle(u.title, i + 1)).join('\n')}

Today we will be upgrading all JavaScript files using Flow in: ${chalk.bold.magenta(
      directory,
    )}

Excluding any JavaScript files in node_modules directories, in flow-typed
directories, in __flowtests__ directories, or files ending in -flowtest.js.
`.slice(1);
    console.log(message);
  }
  // Create a new spinner.
  const spinner = ora({
    text: chalk.italic.cyan('Finding all the Flow files to be upgraded...'),
    color: 'cyan',
  });
  // Start the spinner.
  spinner.start();
  // Find all of the Flow files in the directory we are upgrading.
  let filePaths;
  try {
    filePaths = await findFlowFiles(directory, options);
  } catch (error) {
    // Stop the spinner if we get an error.
    spinner.stop();
    throw error;
  }
  // Stop the spinner.
  spinner.stop();
  // Log the number of Flow files that we found.
  console.log(`Found ${chalk.bold.cyan(filePaths.length)} Flow files.`);
  console.log();
  // Make sure the user knows that they should review their information before
  // proceeding.
  console.log(
    chalk.bold.red(
      'Please review this information to make sure that it is correct before proceeding.',
    ),
  );
  console.log();
  // Ask the user for confirmation.
  const answer = await new Confirm('Are you ready to begin the upgrade?').run();
  // If the user did not let us continue then log a new line and do not
  // continue.
  if (!answer) {
    console.log();
    return;
  }
  // Run through all of our upgrades.
  while (allUpgrades.length) {
    // Put some space between us and the last upgrade.
    console.log();
    // Get the next upgrade.
    const upgrade = allUpgrades.shift();
    switch (upgrade.kind) {
      // If this is a codemod, collect all codemods after it as well and execute
      // those codemods together.
      case 'codemod': {
        const codemods = [upgrade];
        // While the next upgrade is a codemod, add it to our `codemods` array.
        while (allUpgrades[0] && allUpgrades[0].kind === 'codemod') {
          codemods.push(allUpgrades.shift());
        }
        // Tell the user what we are about to do.
        console.log(Styled.sectionHeader('Codemods'));
        console.log();
        console.log('We are now going to run the following codemods:');
        console.log();
        console.log(
          codemods
            .map((u, i) => Styled.upgradeTitle(u.title, i + 1))
            .join('\n'),
        );
        console.log();
        console.log(
          'Following is a short description of each codemod listed above so ' +
            'that you know\nhow your code is being upgraded.',
        );
        console.log();
        console.log(Styled.divider());
        console.log();
        codemods.forEach((u, i) => {
          console.log(Styled.upgradeTitle(u.title, i + 1));
          console.log();
          console.log(u.description);
          console.log();
          console.log(Styled.divider());
          console.log();
        });
        console.log(`Running ${chalk.bold.cyan(codemods.length)} codemods...`);
        console.log();
        // Run all of our codemods.
        const runCodemods = require('./codemods/runCodemods');
        await runCodemods(
          codemods.map(({transformPath}) => transformPath),
          filePaths,
        );
        break;
      }
      // Throw an error for all other kinds of upgrades.
      default:
        throw new Error(`Unexpected upgrade kind: '${upgrade.kind}'`);
    }
  }
};
