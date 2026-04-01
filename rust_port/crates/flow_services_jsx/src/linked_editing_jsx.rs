/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::ast;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;

struct Found(Vec<Loc>);

fn loc_of_jsx_name(name: &ast::jsx::Name<Loc, Loc>) -> &Loc {
    match name {
        ast::jsx::Name::Identifier(id) => &id.loc,
        ast::jsx::Name::NamespacedName(ns) => &ns.loc,
        ast::jsx::Name::MemberExpression(mem) => &mem.loc,
    }
}

fn name_loc_of_jsx_fragment_element_loc(frag_elem_loc: &Loc) -> Loc {
    let end_pos = Position {
        line: frag_elem_loc.end.line,
        column: frag_elem_loc.end.column - 1,
    };
    Loc {
        source: frag_elem_loc.source.dupe(),
        start: end_pos,
        end: end_pos,
    }
}

struct ContainsMapper {
    target: Loc,
}

impl<'ast> AstVisitor<'ast, Loc, Loc, &'ast Loc, Found> for ContainsMapper {
    fn normalize_loc(loc: &'ast Loc) -> &'ast Loc {
        loc
    }

    fn normalize_type(type_: &'ast Loc) -> &'ast Loc {
        type_
    }

    fn jsx_element(
        &mut self,
        _loc: &'ast Loc,
        elem: &'ast ast::jsx::Element<Loc, Loc>,
    ) -> Result<(), Found> {
        match &elem.closing_element {
            Some(closing) => {
                let opening_loc = loc_of_jsx_name(&elem.opening_element.name);
                let closing_loc = loc_of_jsx_name(&closing.name);
                if opening_loc.contains(&self.target) {
                    return Err(Found(vec![opening_loc.dupe(), closing_loc.dupe()]));
                }
                ast_visitor::jsx_element_default(self, _loc, elem)
            }
            None => ast_visitor::jsx_element_default(self, _loc, elem),
        }
    }

    fn jsx_fragment(
        &mut self,
        loc: &'ast Loc,
        frag: &'ast ast::jsx::Fragment<Loc, Loc>,
    ) -> Result<(), Found> {
        let opening_loc = name_loc_of_jsx_fragment_element_loc(&frag.frag_opening_element);
        let closing_loc = name_loc_of_jsx_fragment_element_loc(&frag.frag_closing_element);
        if opening_loc.contains(&self.target) {
            return Err(Found(vec![opening_loc, closing_loc]));
        }
        ast_visitor::jsx_fragment_default(self, loc, frag)
    }
}

pub fn get_linked_locs(ast: &ast::Program<Loc, Loc>, target_loc: Loc) -> Option<Vec<Loc>> {
    let mut mapper = ContainsMapper { target: target_loc };
    match mapper.program(ast) {
        Err(Found(locs)) => Some(locs),
        Ok(()) => None,
    }
}
