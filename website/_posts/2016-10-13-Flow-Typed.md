---
title: Introducing Flow-Typed
short-title: Introducing Flow-Typed 
author: jeffmo
---

Having high-quality and community-driven library definitions (“libdefs”) are important for having a great experience with Flow. Today, we are introducing **flow-typed**: A [repository](https://github.com/flowtype/flow-typed/) and [CLI tool](http://npmjs.org/packages/flow-typed) that represent the first parts of a new workflow for building, sharing, and distributing Flow libdefs.

The goal of this project is to grow an ecosystem of libdefs that [allows Flow's type inference to shine](https://medium.com/@thejameskyle/flow-mapping-an-object-373d64c44592) and that aligns with Flow's mission: To extract precise and *accurate* types from real-world JavaScript. We've learned a lot from similar efforts like DefinitelyTyped for TypeScript and we want to bring some of the lessons we've learned to the Flow ecosystem.

Here are some of the objectives of this project:

<!--truncate-->

* Libdefs should be **versioned** — both against the libraries they describe *and* against the version(s) of Flow they are compatible with.
* Libdefs should meet a **high quality bar**, including **libdef tests** to ensure that their quality persists over time.
* There must be a straightforward way to **contribute libdef improvements over time** and for developers to **benefit from those improvements** over time.
* The process of managing libdefs for a Flow project should be **automated, simple, and easy to get right**.

## Versioned & Tested Libdefs

Anyone can contribute a libdef (or improve on an existing one), but when doing so it's important that we maintain a high quality bar so that all developers feel confident in the libdefs they are using. To address this, flow-typed requires that all libdef contributions are explicitly versioned against both the version of the library they are describing and the version(s) of Flow the libdef is compatible with.

Additionally, all libdefs must be accompanied by tests that exercise the important parts of the API and assert that they yield the correct types. By including both version information and tests with each libdef, we can automatically verify in Travis that the tests work as expected for all versions of Flow a libdef is compatible with. Tests also help to ensure that future changes to the libdef don't regress its features over time.

## Automating Libdef Installation

We've built a simple CLI tool called `flow-typed` that helps to automate the process of finding, installing, and upgrading libdefs in your Flow projects. It uses the explicit version info associated with each libdef to find all necessary libdefs based on your project's package.json dependencies. This minimizes the work you need to do in order to pull in and update libdefs in your projects.

You can get the flow-typed CLI using either yarn (`yarn global add flow-typed`) or npm (`npm install -g flow-typed`).

## Installing Libdefs

Installing libdefs from the flow-typed repository is a matter of running a single command on your project after installing your dependencies:

```
> yarn install # Or `npm install` if you're old-school :)
> flow-typed install
```

The `flow-typed install` command reads your project's package.json file, queries the flow-typed repository for libdefs matching your dependencies, and installs the correctly-versioned libdefs into the `flow-typed/` directory for you. By default, Flow knows to look in the `flow-typed/` directory for libdefs — so there is no additional configuration necessary.

Note that it's necessary to run this command *after* running `yarn` or `npm install`. This is because this command will also generate stub libdefs for you if one of your dependencies doesn't have types.

Once libdefs have been installed, **we recommend that you check them in to your project's repo**. Libdefs in the flow-typed repository may be improved over time (fixing a bug, more precise types, etc). If this happens for a libdef that you depend on, you'll want to have control over when that update is applied to your project. Periodically you can run `flow-typed update` to download any libdef updates, verify that your project still typechecks, and the commit the updates.

## Why Not Just Use Npm To Distribute Libdefs?

Over time libdefs in the flow-typed repo may be updated to fix bugs, improve accuracy, or make use of new Flow features that better describe the types of the library. As a result, there are really 3 versions that apply to each libdef: The version of the library being described, the current version of the libdef in the flow-typed repo, and the version(s) of Flow the libdef is compatible with.

If an update is made to some libdef that you use in your project after you've already installed it, there's a good chance that update may find new type errors in your project that were previously unknown. While it is certainly a good thing to find errors that were previously missed, you'll want to have control over when those changes get pulled in to your project.

This is the reason we advise that you commit your installed libdefs to version control rather than rely on a system like npm+semver to download and install a non-deterministic semver-ranged version from npm. Checking in your libdefs ensures that all collaborators on your project have consistent output from Flow at any given commit in version history.

## Building a Community

This is first and foremost a community project. It was started by a community member (hey [@splodingsocks](https://github.com/splodingsocks)!) and has already benefitted from hours of work by many others. Moreover, this will continue to be a community effort: Anyone can create and/or help maintain a libdef for any npm library. Authors may create libdefs for their packages when publishing, and/or consumers can create them when someone else hasn't already done so. Either way, everyone benefits!

We'd like to send a big shout-out to [@marudor](https://github.com/marudor) for contributing so many of his own libdefs and spending time helping others to write and contribute libdefs. Additionally we'd like to thank [@ryyppy](https://github.com/ryyppy) for helping to design and iterate on the CLI and installation workflow as well as manage libdef reviews.

The Flow core team intends to stay invested in developing and improving this project, but in order for it to truly succeed we need your help! If you've already written some libdefs for Flow projects that you work on, we encourage you to [contribute](https://github.com/flowtype/flow-typed/#how-do-i-contribute-library-definitions) them for others to benefit from them as well. By managing libdefs in a community-driven repository, the community as a whole can work together to extend Flow's capabilities beyond just explicitly-typed JS.

It's still early days and there's still a lot to do, so we're excited to hear your ideas/feedback and read your pull requests! :)

Happy typing!
