/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde_json::Value;

use crate::comment::get_path_to_loc::PathNode;
use crate::comment::get_path_to_loc::node_type;
use crate::flow_result::FlowLoc;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Context {
    Normal,
    Jsx,
    JsxFragment,
    Template,
}

pub(crate) fn get_context<'a>(loc: &FlowLoc, path: &'a [PathNode<'a>]) -> (Context, &'a Value) {
    let mut inside = Context::Normal;
    let mut ast = path[0].ast;
    let mut index = 0;
    while index < path.len() {
        ast = path[index].ast;
        if loc_start_line(ast).is_some_and(|line| line >= loc.start.line) {
            // We've reached the line
            break;
        }

        if index < path.len() - 1
            && node_type(ast) == Some("JSXElement")
            && path[index + 1].key == "children"
        {
            // We've entered a JSX children block

            // for errors that span the entire children block, the error starts
            // on the character after the opening tag, meaning the suppression
            // will end up inside the tag rather than inside the child:
            //
            //  <Foo
            //    bar="baz">
            //              ^ error starts here
            // v error ends here
            //  </Foo>
            //
            // so, if the start line of the error is <= the ending line
            // of the opening tag, the suppression is not inside JSX.
            if ast
                .get("openingElement")
                .and_then(loc_end_line)
                .is_some_and(|line| line < loc.start.line)
            {
                inside = Context::Jsx;
            }
            index += 2;
            continue;
        }

        if index < path.len() - 1
            && node_type(ast) == Some("JSXFragment")
            && path[index + 1].key == "children"
        {
            // We've entered a JSX fragment block
            inside = Context::JsxFragment;
            index += 2;
            continue;
        }

        if index < path.len() - 1
            && node_type(ast) == Some("TemplateLiteral")
            && path[index + 1].key == "expressions"
        {
            // We've entered a template string
            inside = Context::Template;
            index += 2;
            continue;
        }

        if inside != Context::Jsx || node_type(ast) != Some("JSXText") {
            inside = Context::Normal;
        }
        index += 1;
    }

    (inside, ast)
}

fn loc_start_line(value: &Value) -> Option<i64> {
    value.get("loc")?.get("start")?.get("line")?.as_i64()
}

fn loc_end_line(value: &Value) -> Option<i64> {
    value.get("loc")?.get("end")?.get("line")?.as_i64()
}
