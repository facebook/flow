// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use dupe::Dupe;
use flow_parser::estree_translator;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::offset_utils::OffsetTable;
use flow_parser_utils::ast_builder;

use crate::indented_printer;

fn emit(source: &str) -> String {
    let (_, errors) = flow_parser::parse_program_file::<()>(
        false,
        None,
        None,
        FileKey::new(FileKeyInner::SourceFile("printer.js".to_owned())),
        Ok(source),
    );
    assert!(errors.is_empty(), "invalid test input {source}: {errors:?}");
    let program = ast_builder::test_program_of_string(source);
    indented_printer::print(&program).unwrap().contents()
}

fn assert_round_trip(source: &str) {
    let output = emit(source);
    let (_, errors) = flow_parser::parse_program_file::<()>(
        false,
        None,
        None,
        FileKey::new(FileKeyInner::SourceFile("printer.js".to_owned())),
        Ok(&output),
    );
    assert!(
        errors.is_empty(),
        "invalid printed program {output}: {errors:?}"
    );
    let input_comments = ast_builder::test_program_of_string(source).all_comments;
    let output_comments = ast_builder::test_program_of_string(&output).all_comments;
    let mut expected_comments: Vec<_> = input_comments
        .iter()
        .map(|comment| (&comment.kind, &comment.text))
        .collect();
    let mut actual_comments: Vec<_> = output_comments
        .iter()
        .map(|comment| (&comment.kind, &comment.text))
        .collect();
    expected_comments.sort();
    actual_comments.sort();
    assert_eq!(
        expected_comments, actual_comments,
        "printed comments: {output}"
    );
    let config = estree_translator::Config {
        include_locs: false,
        include_filename: false,
        offset_style: estree_translator::OffsetStyle::JsIndices,
    };
    let mut expected = estree_translator::program(
        &OffsetTable::make(source),
        &config,
        &ast_builder::test_program_of_string(source),
    );
    let mut actual = estree_translator::program(
        &OffsetTable::make(&output),
        &config,
        &ast_builder::test_program_of_string(&output),
    );
    for ast in [&mut expected, &mut actual] {
        let mut pending = vec![ast];
        while let Some(node) = pending.pop() {
            if node.is_object() {
                let fields = node.as_object_mut().expect("checked object");
                if fields.get("type").and_then(|value| value.as_str()) == Some("Literal") {
                    fields.remove("raw");
                }
                fields.remove("comments");
                fields.remove("leadingComments");
                fields.remove("trailingComments");
                fields.remove("innerComments");
                pending.extend(fields.values_mut());
            } else if node.is_array() {
                pending.extend(node.as_array_mut().expect("checked array"));
            }
        }
    }
    assert_eq!(expected, actual, "printed program: {output}");
}

#[test]
fn indents_blocks_and_keeps_return_arguments_on_the_same_line() {
    assert_eq!(
        emit("function f() { if (ready) { return value; } else { throw error; } }"),
        "function f() {\n  if (ready) {\n    return (value);\n  } else {\n    throw (error);\n  }\n}\n"
    );
}

#[test]
fn parentheses_and_comments_preserve_expression_structure() {
    for source in [
        "function f() { return (a + b) * (c - d); }",
        "function f() { return (// value\n a?.b ?? c); }",
        "function f() { throw (// failure\n new Error('bad')); }",
        "const f = x => ({value: x}); const v = (new Factory()).method();",
        "for ((key in object); ready; next()) { if (done) break; }",
        "const a = x + +y; const b = x - -y; const c = /x+/g.test(value);",
        "const o = { method(...args) { return fn(...args); }, get value() { return x; } };",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn preserves_template_text_and_escaped_directives() {
    for source in [
        "function f(x) { return `first\n${x}\n  last`; }",
        "function f(x) { return tag`first\n${x}\n  last`; }",
        "function f() { 'use\\x20strict'; with (object) { return value; } }",
        "const text = '😀\\n'; const n = 1e3; const big = 123n;",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn line_breaks_do_not_depend_on_identifier_width() {
    let short = emit("function f(value) { return value + other; }");
    let long_name = "value".repeat(100);
    let long = emit(&format!(
        "function f({long_name}) {{ return {long_name} + other; }}"
    ));
    assert_eq!(short, long.replace(&long_name, "value"));
}

#[test]
fn preserves_every_runtime_statement_form() {
    for source in [
        "var a = 1, b; let c; const d = 2; ; debugger;",
        "if (a) if (b) first(); else second(); else third();",
        "if (a) { if (b) first(); } else second();",
        "outer: for (let i = 0; i < 10; i++) { if (i) continue outer; break outer; }",
        "for (;;) ; for (key in object) visit(key); for (const key in object) {}",
        "for (const value of values) visit(value); for ({value} of values) {}",
        "while (ready) next(); do next(); while (ready); do {} while (false);",
        "switch (value) { case a + b: first(); break; case 2: case 3: second(); default: last(); }",
        "try { work(); } catch ({message}) { recover(message); } finally { cleanup(); }",
        "try {} catch {} try {} finally {} with (object) { value; }",
        "function f() { return; } function g() { throw new Error('bad'); }",
        "function f() { if (a) function g() {} }",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn preserves_operator_trees_and_assignment_targets() {
    for source in [
        "a + b * c ** (d ** e); (a ** b) ** c; (-a) ** b; a - (b - c);",
        "a < b; a <= b; a > b; a >= b; a == b; a != b; a === b; a !== b;",
        "a << b; a >> b; a >>> b; a | b; a ^ b; a & b; a in b; a instanceof b;",
        "a && (b || c); (a ?? b) || c; a ?? (b && c); a ? b ? c : d : e;",
        "a = b = c; a += b; a -= b; a *= b; a /= b; a %= b; a **= b;",
        "a <<= b; a >>= b; a >>>= b; a |= b; a ^= b; a &= b; a &&= b; a ||= b; a ??= b;",
        "typeof x; void x; delete object.key; ~x; !x; +x; -x; ++x; x++; --object.x; object.x--;",
        "({a, b: object.b, [key]: target = 1, ...rest} = source); [a, , ...object.rest] = values;",
        "(first(), second(), third()); const a = (first(), second());",
        "async function f() { return await work(); } function* g() { yield; yield value; yield* values; }",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn preserves_optional_chains_and_explicit_boundaries() {
    for source in [
        "object?.a.b(); object?.a().b; object.a?.(value).b; object?.[key]?.(value)?.[other];",
        "(object?.a).b; (object?.a)(); (object?.a()).b; (object?.a)?.b;",
        "(object?.a).b?.c; (object?.a())?.b(); object?.a.b?.c.d();",
        "new Factory; new Factory(); new (factory())(value); (new Factory()).method();",
        "eval('value'); (0, eval)('value'); object.method(); (object.method)();",
        "1..toString(); /x+/g.test(value); null; true; false; this;",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn preserves_destructuring_objects_arrays_and_methods() {
    for source in [
        "const a = []; const b = [,]; const c = [,,]; const d = [1,,]; const e = [...values];",
        "const [,a = 1,,...rest] = values; const {a, b: c = 2, [key]: d, ...others} = object;",
        "function f({a = 1}, [b,,], c = (a,b), ...rest) { return rest; }",
        "const object = {a, b: value, ['x']: 1, 1: 2, 3n: 4, ...other, __proto__: prototype};",
        "const object = {__proto__() {}, get x() { return 1; }, set x(value) {}, async f() {}, *g() { yield 1; }, async *h() { yield 2; }};",
        "const f = async (a = 1, ...rest) => ({a, rest}); const g = () => (first(), second());",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn preserves_classes_private_names_and_super() {
    for source in [
        "class C { x; y = 1; #z = 2; static n; static p = 3; static #q; }",
        "class C extends Base { constructor(...args) { super(...args); } f() { return super.f(); } static g() { return super.value; } }",
        "class C { get x() { return this.#x; } set x(value) { this.#x = value; } #x; #method() {} async f() {} *g() {} async *h() {} }",
        "class C { static { this.x = 1; } static [key]() {} [key + suffix] = value; f() { return this.#x; } #x; }",
        "const C = class extends (factory()) { f() { return new.target; } };",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn preserves_modules_and_import_attributes() {
    for source in [
        "import 'module'; import value from 'module'; import * as ns from 'module';",
        "import value, {a, b as c, d} from 'module';",
        "import value, * as ns from 'module'; import {} from 'module';",
        "import value from 'module' with {type: 'json', 'mode': 'x'};",
        "export const x = 1; export function f() {} export class C {}",
        "const x = 1; export {x, x as y, x as z}; export {x as remote} from 'module';",
        "export * from 'module'; export * as ns from 'module'; export {} from 'module';",
        "export default function() {}",
        "export default class {}",
        "export default (a + b); import('module', {with: {type: 'json'}}); import.meta.url;",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn preserves_jsx_text_and_attribute_entities() {
    for source in [
        "const element = <A.B.C flag key='&quot;' other={value} {...props}>first\n  last {value}<ns:tag ns:key=\"&amp;\" />{/*keep*/}</A.B.C>;",
        "function f() { return <>first\n    last<A />{...values}</>; }",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn comments_preserve_restricted_productions() {
    for source in [
        "function f() { return (// before\nvalue // after\n); }",
        "function f() { throw (// before\nnew Error('bad') // after\n); }",
        "const f = async (/*before*/value /*after*/) /*arrow*/ => value;",
        "function f() { return (value // update\n)++; }",
        "label: while (value) { break /*label*/ label; }",
        "const f = async /*async*/ function /*name*/ f() {};",
        "const object = {async /*key*/ f() {}, get /*key*/ x() { return 1; }};",
        "class C { static /*key*/ async /*async*/ f() {} }",
    ] {
        assert_round_trip(source);
    }
}

#[test]
fn refuses_unlowered_types_and_runtime_flow_syntax() {
    for source in [
        "type T = number;",
        "const x: number = 1;",
        "enum E {A}",
        "record R {x: number}",
        "component C() { return null; }",
        "const x = (value: number);",
    ] {
        let program = ast_builder::test_program_of_string(source);
        assert!(
            indented_printer::print(&program).is_err(),
            "unlowered source: {source}"
        );
    }
}

#[test]
fn fences_dangling_else_in_constructed_ast() {
    use std::sync::Arc;

    use flow_parser::ast::statement::Statement;
    use flow_parser::ast::statement::StatementInner;
    use flow_parser::ast::statement::{self};
    let mut program = ast_builder::test_program_of_string(
        "if (outer) while (ready) if (inner) first(); second();",
    );
    let StatementInner::If { loc, inner } = &*program.statements[0] else {
        panic!("expected if");
    };
    let mut outer = (**inner).clone();
    outer.alternate = Some(statement::if_::Alternate {
        loc: program.statements[1].loc().dupe(),
        body: program.statements[1].dupe(),
        comments: None,
    });
    program.statements = Arc::from([Statement::new(StatementInner::If {
        loc: loc.dupe(),
        inner: Arc::new(outer),
    })]);
    let output = indented_printer::print(&program).unwrap().contents();
    assert_round_trip(&output);
    let printed = ast_builder::test_program_of_string(&output);
    let StatementInner::If { inner, .. } = &*printed.statements[0] else {
        panic!("expected outer if");
    };
    assert!(inner.alternate.is_some());
    assert!(matches!(&*inner.consequent, StatementInner::Block { .. }));
}
