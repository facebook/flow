/*
 * @flow
 */


import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

const files = [
  'other/explicitly_included.js',
  'src/implicitly_included.js',
  'src/explicitly_ignored.js',
  'other/implicitly_ignored.js',
  'src/explicit_lib.js',
  'src/flow-typed/implicit_lib.js',
];

module.exports = (suite(({addFile, flowCmd, removeFile}) => [
  test('No --all flag and implicit root', [
    flowCmd(['ls'])
      .stderr(
        `
          Could not find a .flowconfig in . or any of its parent directories.
          See "flow init --help" for more info
        `,
      )
      .sortedStdout('')
      .because("Assumes current directory is root, and there's no .flowconfig"),

    flowCmd(['ls', '--strip-root', 'src'])
      .stderr('')
      .sortedStdout(
        `
          .flowconfig
          explicit_lib.js
          flow-typed/implicit_lib.js
          implicitly_included.js
        `,
      )
      .because('Infers root and only shows included files in src directory'),

    flowCmd(['ls', '--strip-root', 'src', 'other'])
      .stderr('')
      .sortedStdout(
        `
          ../other/explicitly_included.js
          .flowconfig
          explicit_lib.js
          flow-typed/implicit_lib.js
          implicitly_included.js
        `,
      )
      .because('Infers root and will show included files in both directories'),

    flowCmd(['ls', '--strip-root', 'other', 'src'])
      .stderr(
        `
          Could not find a .flowconfig in other or any of its parent directories.
          See "flow init --help" for more info
        `,
      )
      .sortedStdout('')
      .because('Infers root from first arg, which is not a flow root'),

    flowCmd(['ls', '--strip-root', 'src/doesNotExist.js'])
      .sortedStdout('')
      .sortedStdout('')
      .because("Won't show files that don't exist")
  ]),
  test('Explicit root will not filter out files in other/',[
    flowCmd([
      'ls',
      '--strip-root',
      '--root',
      'src',
    ])
      .stderr('')
      .sortedStdout(
        `
          ../other/explicitly_included.js
          .flowconfig
          explicit_lib.js
          flow-typed/implicit_lib.js
          implicitly_included.js
        `,
      ),
  ]),
  test('--all should all libs, included files, and explicitly ignored files', [
    flowCmd([
      'ls',
      '--strip-root',
      '--all',
      '--root',
      'src',
    ])
      .stderr('')
      .sortedStdout(
        `
          ../other/explicitly_included.js
          .flowconfig
          explicit_lib.js
          explicitly_ignored.js
          flow-typed/implicit_lib.js
          implicitly_included.js
        `,
      ),
  ]),
  test('Implicit/Explicit Included/Ignored/Lib should be correct', [
      flowCmd([
        'ls',
        '--strip-root',
        '--root', 'src',
        '--all',
        '--explain',
      ].concat(files)) // Explicitly list out files
        .stderr('')
        .sortedStdout(
          `
            ExplicitLib           explicit_lib.js
            ExplicitlyIgnored     explicitly_ignored.js
            ExplicitlyIncluded    ../other/explicitly_included.js
            ImplicitLib           flow-typed/implicit_lib.js
            ImplicitlyIgnored     ../other/implicitly_ignored.js
            ImplicitlyIncluded    implicitly_included.js
          `,
        ),
  ]),
  test('JSON output without --explain should be an array', [
    flowCmd([
      'ls',
       '--json',
       '--strip-root',
       '--root', 'src',
       '--all',
     ].concat(files).concat(['src/.flowconfig']))
      .stderr('')
      .stdout(
        `
          [
            "../other/explicitly_included.js",
            "../other/implicitly_ignored.js",
            ".flowconfig",
            "explicit_lib.js",
            "explicitly_ignored.js",
            "flow-typed/implicit_lib.js",
            "implicitly_included.js"
          ]
        `,
      ),
  ]),
  test('JSON output with --explain should be JSON object',[
    flowCmd([
      'ls',
       '--json',
       '--strip-root',
       '--root', 'src',
       '--all',
       '--explain'
     ].concat(files).concat(['src/.flowconfig']))
      .stderr('')
      .stdout(
        `
          {
            "../other/explicitly_included.js": {
              "explanation": "ExplicitlyIncluded"
            },
            "../other/implicitly_ignored.js": {
              "explanation": "ImplicitlyIgnored"
            },
            ".flowconfig": {
              "explanation": "ConfigFile"
            },
            "explicit_lib.js": {
              "explanation": "ExplicitLib"
            },
            "explicitly_ignored.js": {
              "explanation": "ExplicitlyIgnored"
            },
            "flow-typed/implicit_lib.js": {
              "explanation": "ImplicitLib"
            },
            "implicitly_included.js": {
              "explanation": "ImplicitlyIncluded"
            }
          }
        `,
      ),
  ]),
  test('Listing files over stdin', [
    addFile('stdin_file.txt'),
    flowCmd(['ls', '--strip-root', '--root', 'src', '--all', '--input-file', '-'], 'stdin_file.txt')
      .stderr('')
      .sortedStdout(
        `
          ../other/explicitly_included.js
          .flowconfig
          explicit_lib.js
          explicitly_ignored.js
          flow-typed/implicit_lib.js
          implicitly_included.js
        `,
      )
      .because('Same as if we passed src/ and other/explicitly_include.js from the command line'),

    flowCmd(['ls', '--strip-root', '--root', 'src', '--all', '--input-file', '-', 'other/implicitly_ignored.js'], 'stdin_file.txt')
      .stderr('')
      .sortedStdout(
        `
          ../other/explicitly_included.js
          ../other/implicitly_ignored.js
          .flowconfig
          explicit_lib.js
          explicitly_ignored.js
          flow-typed/implicit_lib.js
          implicitly_included.js
        `,
      )
      .because('flow ls will combine command line with the input file'),
  ]),
  test('Input file on disk', [
    addFile('stdin_file.txt'),
    flowCmd(['ls', '--strip-root', '--root', 'src', '--all', '--input-file', 'stdin_file.txt'])
      .stderr('')
      .sortedStdout(
        `
          ../other/explicitly_included.js
          .flowconfig
          explicit_lib.js
          explicitly_ignored.js
          flow-typed/implicit_lib.js
          implicitly_included.js
        `,
      )
      .because('Same as if we passed src/ and other/explicitly_include.js from the command line'),

    flowCmd(['ls', '--strip-root', '--root', 'src', '--all', '--input-file', 'stdin_file.txt', 'other/implicitly_ignored.js'])
      .stderr('')
      .sortedStdout(
        `
          ../other/explicitly_included.js
          ../other/implicitly_ignored.js
          .flowconfig
          explicit_lib.js
          explicitly_ignored.js
          flow-typed/implicit_lib.js
          implicitly_included.js
        `,
      )
      .because('flow ls will combine command line with the input file'),
  ]),
  test('Non-existent files and directories', [
    flowCmd(['ls', '--strip-root', 'src/foobar'])
      .stderr(
        `
          Could not find file or directory src/foobar; canceling search for .flowconfig.
          See "flow init --help" for more info

        `,
      )
      .because('We try to use foobar to infer the root, so we complain when it doesnt exist'),
    flowCmd(['ls', '--strip-root', '--root', 'src', 'src/foobar', 'src/implicitly_included.js'])
      .stderr(``)
      .stdout(
        `
          implicitly_included.js

        `,
      )
      .because('We just filter out non-existent files'),
    flowCmd(['ls', '--strip-root', '--imaginary', '--root', 'src', 'src/foobar', 'src/implicitly_included.js', 'src/flow-typed/baz.js'])
      .stderr(``)
      .stdout(
        `
          foobar
          implicitly_included.js
          flow-typed/baz.js

        `,
      )
      .because('With --imaginary we include non-existent files. Non-existent files are never considered to be libs.'),
    flowCmd(['ls', '--strip-root', '--explain', '--imaginary', '--root', 'src', 'src/foobar', 'src/baz', 'src/implicitly_included.js', 'src/flow-typed/baz'])
      .stderr(``)
      .stdout(
        `
          ImplicitlyIncluded    foobar
          ImplicitlyIncluded    baz
          ImplicitlyIncluded    implicitly_included.js
          ImplicitlyIncluded    flow-typed/baz

        `,
      )
      .because('--explain should work with --imaginary as expected. Non-existent files are never considered to be libs.'),
    flowCmd(['ls', '--all', '--strip-root', '--root', 'src', 'src/foobar', 'src/implicitly_included.js'])
      .stderr(``)
      .stdout(
        `
          implicitly_included.js

        `,
      )
      .because('We just filter out non-existent files. --all does not imply --imaginary'),
    flowCmd(['ls', '--all', '--imaginary', '--strip-root', '--explain', '--root', 'src', 'foobar', 'src/foobar', 'src/implicitly_included.js'])
      .stderr(``)
      .stdout(
        `
          ImplicitlyIgnored     ../foobar
          ImplicitlyIncluded    foobar
          ImplicitlyIncluded    implicitly_included.js

        `,
      )
      .because('../foobar is implicitly ignored and only listed with the --all flag'),
  ]),
]).beforeEach(({addFile, addFiles, removeFile}) => [
  addFile('src/_flowconfig', 'src/.flowconfig')
    .addFiles(...files)
    .removeFile('.flowconfig')
]): Suite);
