/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const core = require('@actions/core');
const exec = require('@actions/exec');
const io = require('@actions/io');
const tc = require('@actions/tool-cache');
const path = require('path');
const uuidV4 = require('uuid/v4');

const CYGWIN_PACKAGES = [
  'rsync',
  'patch',
  'diffutils',
  'curl',
  'make',
  'unzip',
  'git',
  'm4',
  'perl',
  'mingw64-x86_64-gcc-core',
];

async function installWindows() {
  // Install cygwin and the packages that opam needs.
  //
  // TODO: see if this might be faster:
  //
  //   const cygwinInstaller = await tc.downloadTool('https://cygwin.com/setup-x86_64.exe');
  //   await exec.exec(cygwinInstaller, [
  //     '--no-admin',
  //     '--no-shortcuts',
  //     '--packages',
  //     ...CYGWIN_PACKAGES,
  //     '--prune-install',
  //     '--quiet-mode',
  //   ]);
  await exec.exec(`choco.exe install cygwin`);
  await exec.exec(`choco.exe install ${CYGWIN_PACKAGES.join(' ')} --source=cygwin`);

  // download opam installer
  console.log(`Downloading opam`);
  const opamInstaller = await tc.downloadTool('https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2/opam64.tar.xz');

  // extract opam installer. tool-cache's extractTar almost works, except for
  // actions/toolkit#165 and actions/toolkit#180, so we duplicate some of its
  // functionality.
  const tempDirectory = process.env['RUNNER_TEMP'] || path.join(process.env['USERPROFILE'] || 'C:\\', 'flow', 'temp');
  const dest = path.join(tempDirectory, uuidV4());
  await io.mkdirP(dest);
  console.log(`Created temporary directory ${dest}`);
  const tarPath = await io.which('tar', true);
  await exec.exec(`"${tarPath}"`, ["-x", "--force-local", "-f", opamInstaller, "-C", dest.replace(/\\/g, '/')]);

  // run opam installer
  await exec.exec(
    "c:\\tools\\cygwin\\bin\\bash.exe",
    ["opam64/install.sh"],
    {
      cwd: dest,
      env: {
        ...process.env,
        PATH: "c:\\tools\\cygwin\\bin",
      },
    }
  );
}

async function installLinux() {
  const url = 'https://github.com/ocaml/opam/releases/download/2.0.5/opam-2.0.5-x86_64-linux';
  const exe = await tc.downloadTool(url);
  // TODO: chmod +x, mv into PATH
}

async function installMac() {
  const url = 'https://github.com/ocaml/opam/releases/download/2.0.5/opam-2.0.5-x86_64-macos';
  const exe = await tc.downloadTool(url);
  // TODO: chmod +x, mv into PATH
}

async function run() {
  try {
    switch (process.platform) {
      case 'win32':
        await installWindows();
        break;
      case 'linux':
        await installLinux();
        break;
      case 'darwin':
        await installMac();
        break;
      default:
        core.setFailed(`Unsupported platform: ${process.platform}`);
    }
  } catch (error) {
    core.setFailed(error.message);
  }
}

run();
