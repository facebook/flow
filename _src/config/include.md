---
title: .flowconfig [include]
slug: /config/include
description: "Configure which files and directories Flow should include for type checking beyond the project root."
---

## Flow 0.328 and later

The `[include]` section in a `.flowconfig` file tells Flow to include the
specified files or directories. Each line is a glob relative to the project
root. Use `*` to match within one directory and `**` to match across directory
boundaries. To include a directory recursively, end the pattern with `/**`.
Absolute paths and the `<PROJECT_ROOT>` placeholder are not supported.

The project root directory (where your `.flowconfig` lives) is automatically
included.

For example, if `/path/to/root/.flowconfig` contains the following `[include]`
section:

```
[include]
../externalFile.js
../externalDir/**
../otherProject/*.js
../otherProject/**/coolStuff/**
```

Then when Flow checks the project in `/path/to/root`, it will read and watch

1. `/path/to/root/` (automatically included)
2. `/path/to/externalFile.js`
3. Everything under `/path/to/externalDir/`
4. Any file in `/path/to/otherProject/` that ends in `.js`
5. Everything under any directory named `coolStuff/` within
   `/path/to/otherProject`

## Flow 0.327 and earlier

The following path syntax works in Flow 0.325 and earlier.

The `[include]` section in a `.flowconfig` file tells Flow to include the
specified files or directories. Including a directory recursively includes all
the files under that directory. Symlinks are followed as long as they lead to a
file or directory that is also included. Each line in the include section is a
path to include. These paths can be relative to the root directory or absolute,
and support both single and double star wildcards.

The project root directory (where your `.flowconfig` lives) is automatically
included.

For example, if `/path/to/root/.flowconfig` contains the following `[include]`
section:

```
[include]
../externalFile.js
../externalDir/
../otherProject/*.js
../otherProject/**/coolStuff/
```

Then when Flow checks the project in `/path/to/root`, it will read and watch

1. `/path/to/root/` (automatically included)
2. `/path/to/externalFile.js`
3. `/path/to/externalDir/`
4. Any file in `/path/to/otherProject/` that ends in `.js`
5. Any directory under `/path/to/otherProject` named `coolStuff/`
