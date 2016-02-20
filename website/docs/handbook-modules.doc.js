/* @flow */
/*
---
id: handbook/modules
title: Modules
layout: docs
permalink: /docs/handbook/modules.html
---
*/

/*
  A JavaScript project is usually composed of encapsulated "modules," which can
  interact with each other only via explicit "exports" and "imports." Such a
  module system needs two mechanisms to work: (1) a syntax for expressing
  exports and imports of modules; (2) an algorithm to resolve module names to
  files that contain their code.
*/

/*
  For (1), Flow supports the standard ES6 module export/import syntax, as well
  as the popular CommonJS module export/import syntax. For (2), Flow supports
  the popular Node.js module resolution algorithm. In other words, you most
  probably don't have to learn or switch to a new module system just to
  typecheck your JavaScript project with Flow.
*/

/*
  In addition, Flow supports module "declarations" that can be used in lieu of
  actual code for typechecking other parts of a project that depend on them.
  Usually, modules are declared when we don't want to typecheck the code itself,
  or the code is not available for typechecking.
*/

/*
  ## ES6 module export/import syntax

  TODO: link
*/

/*
  ## CommonJS module export/import syntax

  TODO: link
*/

/*
  ## Node.js module resolution algorithm

  TODO: link
*/

/*
  ## Module declarations

  TODO: syntax, precedence rules, link to information on general declarations
  (e.g., `.flowconfig` setup)
*/

/*
  ## Module declarations

  TODO: declare module syntax, .js.flow, precedence rules, link to information
  on general declarations (e.g., `.flowconfig` setup),
*/

/*
  ## Missing annotations

  TODO: examples
*/

function example() { }
