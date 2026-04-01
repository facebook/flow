/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::ast;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser_utils_output::js_layout_generator;
use flow_parser_utils_output::pretty_printer;

struct Found(ast::jsx::Opening<Loc, Loc>);

// Note that this cannot be a Flow_ast_contains_mapper.mapper, because the ending locs of
// jsx elements in error-recovered parses are not correct. For example, if the user types:
//
// <div>
//   <foo>|
// <div>
//
// The end position of the `div` JSXElement is the end position of the opening tag, not the
// closing one, so it doesn't contain the opening `foo` element.
struct Finder {
    target_pos: Position,
}

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, Found> for Finder {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn jsx_opening_element(
        &mut self,
        elem: &'ast ast::jsx::Opening<Loc, Loc>,
    ) -> Result<(), Found> {
        if elem.loc.end == self.target_pos {
            return Err(Found(elem.clone()));
        }
        ast_visitor::jsx_opening_element_default(self, elem)
    }
}

pub fn get_snippet(ast: &ast::Program<Loc, Loc>, target_pos: Position) -> Option<String> {
    let mut finder = Finder { target_pos };
    match finder.program(ast) {
        Ok(()) => None,
        Err(Found(opening)) => {
            if opening.self_closing {
                None
            } else {
                let closing = ast::jsx::Closing {
                    loc: flow_parser::loc::LOC_NONE,
                    name: opening.name.clone(),
                };
                let layout = js_layout_generator::jsx_closing(&closing);
                let source = pretty_printer::print(true, &layout);
                let text = source.contents();
                Some(format!("$0{}", text))
            }
        }
    }
}
