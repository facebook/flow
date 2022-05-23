/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {CliOptions, Upgrade} from './Types';

import semver from 'semver';
import chalk from 'chalk';
import ora from 'ora';
import Confirm from 'prompt-confirm';
import Styled from './Styled';
import {findFlowFilesWithSpinner} from './findFlowFiles';
import runCodemods from './runCodemods';
import {VERSION_UPGRADES} from './upgrades';

/**
 * Decides all of the upgrades that will need to be run when moving from one
 * version to another and runs them asynchronously.
 */
export default async function upgrade(
  directory: string,
  currentVersion: string,
  nextVersion: string,
  options: CliOptions,
): Promise<void> {
  const log: (...messages: $ReadOnlyArray<string>) => void = options.silent
    ? () => {}
    : (...messages) => console.log(...messages);

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
  log();
  log(Styled.sectionHeader('flow-upgrade'));
  log();
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
    log(message);
  }

  // Find all of the Flow files in the directory we are upgrading.
  const filePaths = await findFlowFilesWithSpinner(directory, options);

  // Make sure the user knows that they should review their information before
  // proceeding.
  log(
    chalk.bold.red(
      'Please review this information to make sure that it is correct before proceeding.',
    ),
  );
  log();
  // Ask the user for confirmation.
  const answer =
    options.yes ||
    (await new Confirm('Are you ready to begin the upgrade?').run());
  // If the user did not let us continue then log a new line and do not
  // continue.
  if (!answer) {
    log();
    return;
  }
  // Run through all of our upgrades.
  while (allUpgrades.length) {
    // Put some space between us and the last upgrade.
    log();
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
        log(Styled.sectionHeader('Codemods'));
        log();
        log('We are now going to run the following codemods:');
        log();
        log(
          codemods
            .map((u, i) => Styled.upgradeTitle(u.title, i + 1))
            .join('\n'),
        );
        log();
        log(
          'Following is a short description of each codemod listed above so ' +
            'that you know\nhow your code is being upgraded.',
        );
        log();
        log(Styled.divider());
        log();
        codemods.forEach((u, i) => {
          log(Styled.upgradeTitle(u.title, i + 1));
          log();
          log(u.description);
          log();
          log(Styled.divider());
          log();
        });
        log(`Running ${chalk.bold.cyan(codemods.length)} codemods...`);
        log();
        // Run all of our codemods.
        await runCodemods(codemods, filePaths, options);
        break;
      }
      // Throw an error for all other kinds of upgrades.
      default:
        throw new Error(`Unexpected upgrade kind: '${upgrade.kind}'`);
    }
  }
}
