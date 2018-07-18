# Testgen
This project is a test generator for type checkers such as Flow. It
generates programs with runtime checks, feeds them to a type checker
and runs those programs to see if the type checker will reject bad
programs.

## Building Testgen
To build the executable, run `make`.

## Extending Testgen
`ruleset_base.ml` contains a lot of rules used to generate
programs. However, to accomplish a certain task, one might not want to
use all those rules. Instead, a recommended way to do that is to
inherit the `ruleset_base` class, pick the necessary rules and
possibly overwrite those methods. 

Here are the steps to extend the 
base ruleset and `ruleset_optional.ml`, `ruleset_func.ml`,
`ruleset_depth.ml`, `ruleset_union.ml` and `ruleset_exact.ml` serve as
examples for this extension:

0. Suppose you want to test exact object types and want to generate
   some programs related to exact object types. (`ruleset_exact.ml`
   has all the code)
1. Copy one of the children class of base ruleset, say,
   `ruleset_depth.ml` to `ruleset_exact.ml`. Make classname
   adjustments in there appropriately.
2. Modify the build file
3. Then, in `codegen.ml`, add `ruleset_exact` as one of the engines
4. Compile the program and run
 
## Running Testgen
In the folder where binary lives, run the program once to let it
generate an initial config file called `flowtestgen.json.` Then edit
`flowtestgen.json` to up the number of trials `(num_prog)`. The
typecheck flag controls whether Flow is invoked or not.
Log_to_console controls prints programs to console.

To try random searching,  each ruleset has a random version of that
class as well at the bottom of the file.
