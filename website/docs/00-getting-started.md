---
id: getting-started
title: Getting started with Flow
permalink: /docs/getting-started.html
prev: about-flow.html
next: five-simple-examples.html
---

## Installing Flow

We provide pre-built binaries for Flow, depending on your operating system:

  * [Mac OS X]({{ site.latest_release.mac_binary.url }})
  * [Linux (64 bit)]({{ site.latest_release.linux_binary.url }})

Flow is packaged as a zip file. To install, simply unzip it:

```bash
$> unzip flow.zip
```

This creates a directory called `flow` containing the executable binary (also called `flow`) and a folder of five examples. It's recommended you add this directory to your path so that you can simply run `flow` from anywhere on your system:

```bash
$> cd flow
$> echo -e "\nPATH=\"\$PATH:$(pwd)/\"" >> ~/.bashrc && source ~/.bashrc
```
### Brew on OSX

Alternatively, and more simply, you can install via [brew](http://brew.sh/) on OSX:

```bash
$> brew update
$> brew install flow
```

Brew adds flow to your path as part of the install.

### npm wrapper

You can also install flow by using [flow-bin](https://github.com/flowtype/flow-bin). Similarly, only OS X and Linux (64-bit) binaries are provided.

```bash
$> npm install flow-bin --global
```

### Next steps

To see what Flow can do, let's take a quick look at the [five examples](five-simple-examples.html) in the installation:

1. [Hello Flow!](five-simple-examples.html#1-hello-flow)
2. [Adding type annotations](five-simple-examples.html#2-adding-type-annotations)
3. [Nullable types](five-simple-examples.html#3-nullable-types)
4. [Arrays](five-simple-examples.html#4-arrays)
5. [Dynamic code](five-simple-examples.html#5-dynamic-code)
