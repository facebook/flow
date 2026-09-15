# flow-api-translator

`flow-api-translator` enables creating Flow and TypeScript compatible libraries from Flow source code.

## Usage

This package provides a toolkit of different translation tools to support a number of different JavaScript based language targets from Flow source code.

An important distinction with this translator and others is this is not intended as a migration tool away from Flow, instead the expectation is the source code will continue to be checked by Flow but this translator provides the ability to seamlessly integrate with external Flow, TypeScript and JavaScript based projects.

Below are some common use cases:

### Flow -> Flow Lib

Creating a Flow type definition and runtime files from Flow source code is simple. Just get the source text then pass it to the respective translators:

```
const sourceFile = await fs.readFile('myFile.js', 'utf8');
await fs.writeFile('output/myFile.js.flow', translate.translateFlowToFlowDef(sourceFile, prettierConfig));
await fs.writeFile('output/myFile.js', translate.translateFlowToJS(sourceFile, prettierConfig));
```

### Flow -> TS Lib

Further conversion to TypeScript definition files is supported. To do this, get the source text and pass it to the respective translators:
```
const sourceFile = await fs.readFile('myFile.js', 'utf8');
await fs.writeFile('output/myFile.d.ts', translate.translateFlowToTSDef(sourceFile, prettierConfig));
await fs.writeFile('output/myFile.js', translate.translateFlowToJS(sourceFile, prettierConfig));
```

By default, documentation remains attached to the declaration it originally
described. To associate that documentation with a generated default-export
symbol, pass `defaultExportDocPlacement` to either definition translator:

```
await fs.writeFile(
  'output/myFile.d.ts',
  translate.translateFlowToTSDef(sourceFile, prettierConfig, {
    defaultExportDocPlacement: 'export',
  }),
);
```

The supported placements are `declaration` (the default), `export`, and `both`.
`both` retains the documentation on the original declaration as well as
attaching it to the default export.

### Flow (with Haste modules) -> TS Lib

Haste module paths are commonly used with Flow libraries. TypeScript does not support this so they needs to be converted back to relative paths first:
```
const sourceFile = await fs.readFile('myFile.js', 'utf8');
const mappedSourceFile = translate.translateFlowImportsTo(
  fileContents,
  prettierConfig,
  {sourceMapper: ({module}) => path.relative(currentLocation, ModuleMap.getModulePath(module))},
);
await fs.writeFile('output/myFile.js.flow', translate.translateFlowToTSDef(mappedSourceFile, prettierConfig));
await fs.writeFile('output/myFile.js', translate.translateFlowToJS(mappedSourceFile, prettierConfig));
```
