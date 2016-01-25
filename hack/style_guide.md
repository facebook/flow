Style Guide
===========

As far as indentation rules go, ocp-indent should be used as the final word.

However, here are some examples.

Let Expressions
---------------

Try to fit a `let ... in` onto one line:

    let foo = bar x y z in
    ...

If the expression is too long, first try

    let foo =
      some_long_expression_bar x y z in
      ...

If the second line is still too long, fall back to

    let foo =
      some_long_expression_bar
        long_arg_1
        long_arg_2
        long_arg_3 in

Match Clauses
-------------

Don't indent the clauses of a standalone match expression

    match foo with
    | Some x -> ...
    | None -> ...

However, if the match expression is the RHS of a let expression, do indent them:

    let bar = match foo with
      | Some x -> ...
      | None -> ...
    in
    ...

Alternatively, if `foo` is too long, move the `match` to the next line:

    let foo =
      match some_long_expression arg1 arg2 with
      | Some x -> ...
      | None -> ...
    in
    ...

If the expression between `match ... with` is too long to fit into one line,
split it out as in separate `let` expression. That is, instead of

    let foo = match some_long_expression
      arg1 arg2 with
      | Some x -> ...
      | None -> ...
    in
    ...

Prefer

    let bar = some_long_expression arg1 arg2 in
    let foo = match  bar with
      | Some x -> ...
      | None -> ...
    in
    ...

Note that the `in` should be on its own line. I.e. avoid

    let bar = match foo with
      | Some x -> ...
      | None -> ...  in
    ...

Functions
---------

Function expressions that span multiple lines should be wrapped in a `begin ...  end`:

    List.iter foo begin fun x ->
      ...
    end

However, expressions on a single line should use parentheses:

    List.map foo (fun x -> x + 1)

In general, function bodies should be indented. However, we make an exception
if `fun` comes right after an operator (typically one of `>>=`, `@@`, or `>>|`):

    some_monadic_x >>= fun x ->
    some_monadic_y >>= fun y ->
    return x + y

Modules
-------

Top-level modules (those created by individual files) should be named using
Underscore_separated_words. Modules within files should be CamelCase.

Top-level module opens should be reserved only for very commonly used modules,
like `Utils`. Aside from those, prefer

    module LMN = Long_module_name

or use local opens:

    let open Long_module_name in
    ...

Modules should have the name of their containing directory as a prefix. E.g.
modules under typing/ should be named as `Typing_foo`.

Parentheses
-----------

Generally, there are only two cases where parentheses / `begin ... end` are
necessary: for nesting `with` clauses and for inserting multiple
semicolon-separated expressions into the `then` clause of an `if`-expression.

    match foo with
    | Some x ->
      begin match bar with
       | Some y -> ...
       | None -> ...
      end
    | None -> ...

    if foo then begin
      do_something ();
      do_another_thing ();
    end

If the test expression is too long, the following is also acceptable:

    if long_expression arg1 arg2
    then begin
      do_something ();
      do_another_thing ();
    end

Variants
--------

Variants should be named using `Capitalized_underscore_separated_words`.

This includes polymorphic variants; i.e. we want `\`Foo` and not `\`foo`. In
fact, the OCaml manual suggests that lowercase polymorphic variants may be
deprecated in future versions.

Miscellany
----------

Boolean function parameters should always be labeled. Consider labeling
integer parameters as well.

Try to use exhaustive matching whenever possible. That is, avoid the wildcard
`_` match.
