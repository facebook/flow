/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

const files = [
  'other/explicitly_included.js',
  'src/implicitly_included.js',
  'src/explicitly_ignored.js',
  'other/implicitly_ignored.js',
  'src/explicit_lib.js',
  'src/flow-typed/implicit_lib.js',
];

export default suite(({flowCmd, removeFile}) => [
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

    flowCmd(['ls', 'src'])
      .stderr('')
      .sortedStdout(
        `
          implicitly_included.js

        `,
      )
      .because('Infers root and only shows included files in src directory'),

    flowCmd(['ls', 'src', 'other'])
      .stderr('')
      .sortedStdout(
        `

          ../other/explicitly_included.js
          implicitly_included.js
        `,
      )
      .because('Infers root and will show included files in both directories'),

    flowCmd(['ls', 'other', 'src'])
      .stderr(
        `
          Could not find a .flowconfig in other or any of its parent directories.
          See "flow init --help" for more info
        `,
      )
      .sortedStdout('')
      .because('Infers root from first arg, which is not a flow root'),

    flowCmd(['ls', 'src/doesNotExist.js'])
      .sortedStdout('')
      .sortedStdout('')
      .because("Won't show files that don't exist")
  ]),
  test('Explicit root will not filter',[
    flowCmd([
      'ls',
      '--root',
      'src',
    ])
      .stderr('')
      .sortedStdout(
        `
          ../other/explicitly_included.js
          implicitly_included.js

        `,
      ),
  ]),
  test('--all should all libs, included files, and explicitly ignored files', [
    flowCmd([
      'ls',
      '--all',
      '--root',
      'src',
    ])
      .stderr('')
      .sortedStdout(
        `

          ../other/explicitly_included.js
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
       '--root', 'src',
       '--all',
     ].concat(files))
      .stderr('')
      .stdout(
        `
          [
            "../other/explicitly_included.js",
            "../other/implicitly_ignored.js",
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
       '--root', 'src',
       '--all',
       '--explain'
     ].concat(files))
      .stderr('')
      .stdout(
        `
          {
            "../other/explicitly_included.js": {
              "explanation": "ImplicitlyIgnored"
            },
            "../other/implicitly_ignored.js": {
              "explanation": "ImplicitlyIgnored"
            },
            "explicit_lib.js": {
              "explanation": "ImplicitlyIgnored"
            },
            "explicitly_ignored.js": {
              "explanation": "ImplicitlyIgnored"
            },
            "flow-typed/implicit_lib.js": {
              "explanation": "ImplicitlyIgnored"
            },
            "implicitly_included.js": {
              "explanation": "ImplicitlyIgnored"
            }
          }
        `,
      ),
  ]),
]).beforeEach(({addFile, addFiles, removeFile}) => [
  addFile('src/_flowconfig', 'src/.flowconfig')
    .addFiles(...files)
    .removeFile('.flowconfig')
]);
