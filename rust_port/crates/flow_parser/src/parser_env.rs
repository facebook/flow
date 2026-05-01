/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;
use std::collections::VecDeque;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use crate::ast::*;
use crate::file_key::FileKey;
use crate::flow_lexer::LexResult;
use crate::loc::LOC_NONE;
use crate::loc::Loc;
use crate::loc::Position;
use crate::parse_error::ParseError;
use crate::parser_env::try_parse::Rollback;
use crate::token::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexMode {
    Normal,
    Type,
    JsxTag,
    JsxChild,
    Regexp,
}

pub(crate) mod wrapped_lex_env {
    use dupe::Dupe;

    use crate::file_key::FileKey;
    use crate::lex_env;
    use crate::logos_tokens::MainToken;

    #[derive(Clone)]
    pub(crate) struct WrappedLexEnv<'a> {
        source: &'a str,
        lex_env: lex_env::LexEnv,
        lexer: logos::Lexer<'a, MainToken>,
    }

    impl<'a> WrappedLexEnv<'a> {
        pub(crate) fn new(
            source: Option<FileKey>,
            content: &'a str,
            enable_types_in_comments: bool,
        ) -> Self {
            Self {
                source: content,
                lex_env: lex_env::LexEnv::new(source, enable_types_in_comments),
                lexer: logos::Lexer::new(content),
            }
        }

        pub(crate) fn source(&self) -> Option<FileKey> {
            self.lex_env.source()
        }

        pub(crate) fn lex(&mut self, lex_mode: super::LexMode) -> crate::flow_lexer::LexResult {
            match lex_mode {
                super::LexMode::Normal => {
                    crate::flow_lexer::token(&mut self.lexer, &mut self.lex_env)
                }
                super::LexMode::Type => {
                    crate::flow_lexer::type_token(&mut self.lexer, &mut self.lex_env)
                }
                super::LexMode::JsxTag => {
                    let mut morphed_lexer = self.lexer.clone().morph();
                    let lex_result =
                        crate::flow_lexer::jsx_tag(&mut morphed_lexer, &mut self.lex_env);
                    self.lexer = morphed_lexer.morph();
                    lex_result
                }
                super::LexMode::JsxChild => {
                    let mut morphed_lexer = self.lexer.clone().morph();
                    let lex_result =
                        crate::flow_lexer::jsx_child(&mut morphed_lexer, &mut self.lex_env);
                    self.lexer = morphed_lexer.morph();
                    lex_result
                }
                super::LexMode::Regexp => {
                    let mut morphed_lexer = self.lexer.clone().morph();
                    let lex_result =
                        crate::flow_lexer::regexp(&mut morphed_lexer, &mut self.lex_env);
                    self.lexer = morphed_lexer.morph();
                    lex_result
                }
            }
        }

        pub(crate) fn lex_template_tail_start(&mut self) -> crate::flow_lexer::LexResult {
            let span = self.lexer.span();
            let mut morphed_lexer: logos::Lexer<'a, MainToken> = logos::Lexer::new(self.source);
            morphed_lexer.bump(span.start);
            let mut morphed_lexer = morphed_lexer.morph();
            let prev_last_loc = self.lex_env.last_loc.dupe();
            let lex_result =
                crate::flow_lexer::template_tail_start(&mut morphed_lexer, &mut self.lex_env);
            self.lexer = morphed_lexer.morph();
            self.lex_env.last_loc = prev_last_loc;
            lex_result
        }
    }
}

/// READ THIS BEFORE YOU MODIFY:
///
/// The current implementation for lookahead beyond a single token is inefficient.
/// If you believe you need to increase this constant, do one of the following:
///
/// - Find another way
/// - Benchmark your change and provide convincing evidence that it doesn't
///   actually have a significant perf impact.
/// - Refactor this to memoize all requested lookahead, so we aren't lexing the
///   same token multiple times.
pub(crate) mod lookahead {
    use super::*;

    pub(crate) struct Lookahead<'a> {
        results_0: Option<(wrapped_lex_env::WrappedLexEnv<'a>, LexResult)>,
        results_1: Option<(wrapped_lex_env::WrappedLexEnv<'a>, LexResult)>,
        lex_mode: LexMode,
        lex_env: wrapped_lex_env::WrappedLexEnv<'a>,
    }

    impl<'a> Lookahead<'a> {
        pub(super) fn new(lex_mode: LexMode, lex_env: &wrapped_lex_env::WrappedLexEnv<'a>) -> Self {
            Self {
                results_0: None,
                results_1: None,
                lex_mode,
                lex_env: lex_env.clone(),
            }
        }

        /// precondition: there is enough room in t.la_results for the result
        fn lex(&mut self) {
            let lex_result = self.lex_env.lex(self.lex_mode);
            let cloned_env = self.lex_env.clone();
            match &self.results_0 {
                None => {
                    self.results_0 = Some((cloned_env, lex_result));
                }
                Some(_) => {
                    self.results_1 = Some((cloned_env, lex_result));
                }
            }
        }

        pub(crate) fn peek_0(&mut self) -> &LexResult {
            if self.results_0.is_none() {
                self.lex();
            }
            &self.results_0.as_ref().unwrap().1
        }

        pub(crate) fn peek_1(&mut self) -> &LexResult {
            if self.results_0.is_none() {
                self.lex();
            }
            if self.results_1.is_none() {
                self.lex();
            }
            &self.results_1.as_ref().unwrap().1
        }

        pub(super) fn lex_env_0(&mut self) -> &wrapped_lex_env::WrappedLexEnv<'a> {
            if self.results_0.is_none() {
                self.lex();
            }
            &self.results_0.as_ref().unwrap().0
        }

        /// Throws away the first peeked-at token, shifting any subsequent tokens up
        pub(crate) fn junk(&mut self) {
            if self.results_1.is_none() {
                self.peek_0();
                self.results_0 = None;
            } else {
                let mut tmp = None;
                std::mem::swap(&mut tmp, &mut self.results_1);
                std::mem::swap(&mut tmp, &mut self.results_0);
            }
        }

        pub(super) fn rescan_template_part_from(
            &mut self,
            prev_lex_env: &wrapped_lex_env::WrappedLexEnv<'a>,
        ) {
            let existing_comments = self
                .results_0
                .as_ref()
                .map(|(_, result)| result.comments.clone())
                .unwrap_or_default();
            self.lex_env = prev_lex_env.clone();
            self.results_0 = None;
            self.results_1 = None;
            let mut lex_result = self.lex_env.lex_template_tail_start();
            lex_result.comments = existing_comments;
            let cloned_env = self.lex_env.clone();
            self.results_0 = Some((cloned_env, lex_result));
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenSinkResult {
    pub token_loc: Loc,
    pub token_kind: TokenKind,
    pub token_context: LexMode,
}

#[derive(Debug, Clone)]
pub struct ParseOptions {
    /// enable parsing of Flow component syntax
    pub components: bool,
    /// enable parsing of Flow enums
    pub enums: bool,
    pub pattern_matching: bool,
    pub records: bool,
    /// enable parsing of decorators
    pub esproposal_decorators: bool, /* enable parsing of decorators */
    /// enable parsing of Flow types
    pub types: bool,
    /// treat the file as strict, without needing a "use strict" directive
    pub use_strict: bool,
    pub assert_operator: bool,
    pub module_ref_prefix: Option<FlowSmolStr>,
    /// treat the file as ambient context (e.g. .js.flow or .d.ts)
    pub ambient: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        ParseOptions {
            components: false,
            enums: false,
            assert_operator: false,
            pattern_matching: false,
            records: false,
            esproposal_decorators: false,
            types: true,
            use_strict: false,
            module_ref_prefix: None,
            ambient: false,
        }
    }
}

pub const PERMISSIVE_PARSE_OPTIONS: ParseOptions = ParseOptions {
    components: true,
    enums: true,
    assert_operator: false,
    pattern_matching: true,
    records: true,
    esproposal_decorators: true,
    types: true,
    use_strict: false,
    module_ref_prefix: None,
    ambient: false,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AllowedSuper {
    NoSuper,
    SuperProp,
    SuperPropOrCall,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParserEnvFlags {
    in_strict_mode: bool,
    in_export: bool,
    in_export_default: bool,
    in_loop: bool,
    in_switch: bool,
    in_formal_parameters: bool,
    in_function: bool,
    in_match_expression: bool,
    in_match_statement: bool,
    no_in: bool,
    no_call: bool,
    no_let: bool,
    no_anon_function_type: bool,
    no_conditional_type: bool,
    no_new: bool,
    no_record: bool,
    allow_yield: bool,
    allow_await: bool,
    allow_directive: bool,
    has_simple_parameters: bool,
    allow_super: AllowedSuper,
    in_ambient_context: bool,
}

pub struct ParserEnv<'a> {
    errors: Vec<(Loc, ParseError)>,
    comments: Vec<Comment<Loc>>,
    labels: HashSet<FlowSmolStr>,
    last_lex_result: Option<(Loc, TokenKind)>,
    flags: ParserEnvFlags,
    error_callback: Option<fn(ParseError) -> Result<(), Rollback>>,
    lex_mode_stack: Vec<LexMode>,
    /// lex_env is the lex_env after the single lookahead has been lexed
    lex_env: wrapped_lex_env::WrappedLexEnv<'a>,
    /// This needs to be cleared whenever we advance.
    lookahead: lookahead::Lookahead<'a>,
    token_sink: Option<TokenSink<'a>>,
    parse_options: ParseOptions,
    /* It is a syntax error to reference private fields not in scope. In order to enforce this,
     * we keep track of the privates we've seen declared and used. */
    privates: Vec<(HashSet<String>, Vec<(String, Loc)>)>,
    /* The position up to which comments have been consumed, exclusive. */
    consumed_comments_pos: Position,
}

enum TokenSink<'a> {
    Direct(&'a mut dyn FnMut(TokenSinkResult)),
    Buffered {
        next: Box<TokenSink<'a>>,
        buffer: VecDeque<TokenSinkResult>,
    },
}

fn send_token_to_sink<'a>(token_sink: TokenSink<'a>, token_data: TokenSinkResult) -> TokenSink<'a> {
    match token_sink {
        TokenSink::Direct(token_sink) => {
            token_sink(token_data);
            TokenSink::Direct(token_sink)
        }
        TokenSink::Buffered { next, mut buffer } => {
            buffer.push_back(token_data);
            TokenSink::Buffered { next, buffer }
        }
    }
}

/* constructor */
pub fn init_env<'a, E>(
    token_sink: Option<&'a mut dyn FnMut(TokenSinkResult)>,
    parse_options: Option<ParseOptions>,
    source: Option<FileKey>,
    content: Result<&'a str, E>,
) -> ParserEnv<'a> {
    let mut errors = Vec::new();
    let content = match content {
        Ok(s) => s,
        Err(_) => {
            errors.push((
                Loc {
                    source: source.dupe(),
                    ..LOC_NONE
                },
                ParseError::MalformedUnicode,
            ));
            ""
        }
    };
    let parse_options = parse_options.unwrap_or(PERMISSIVE_PARSE_OPTIONS);
    let enable_types_in_comments = parse_options.types;
    let lex_env = wrapped_lex_env::WrappedLexEnv::new(source, content, enable_types_in_comments);
    let lookahead = lookahead::Lookahead::new(LexMode::Normal, &lex_env);

    ParserEnv {
        errors,
        comments: Vec::new(),
        labels: HashSet::new(),
        last_lex_result: None,
        flags: ParserEnvFlags {
            has_simple_parameters: true,
            in_strict_mode: parse_options.use_strict,
            in_export: false,
            in_export_default: false,
            in_loop: false,
            in_switch: false,
            in_formal_parameters: false,
            in_function: false,
            in_match_expression: false,
            in_match_statement: false,
            no_in: false,
            no_call: false,
            no_let: false,
            no_anon_function_type: false,
            no_conditional_type: false,
            no_new: false,
            no_record: false,
            allow_yield: false,
            allow_await: false,
            allow_directive: false,
            allow_super: AllowedSuper::NoSuper,
            in_ambient_context: false,
        },
        error_callback: None,
        lex_mode_stack: vec![LexMode::Normal],
        lex_env,
        lookahead,
        token_sink: token_sink.map(TokenSink::Direct),
        parse_options,
        privates: Vec::new(),
        consumed_comments_pos: Position { line: 0, column: 0 },
    }
}

impl<'a> ParserEnv<'a> {
    // getters

    pub(crate) fn in_strict_mode(&self) -> bool {
        self.flags.in_strict_mode
    }

    pub(crate) fn lex_mode(&self) -> LexMode {
        self.lex_mode_stack.last().copied().unwrap()
    }

    pub(crate) fn in_export(&self) -> bool {
        self.flags.in_export
    }

    pub(crate) fn in_export_default(&self) -> bool {
        self.flags.in_export_default
    }

    pub(crate) fn comments(&self) -> &Vec<Comment<Loc>> {
        &self.comments
    }

    pub(crate) fn in_labels(&self, label: &str) -> bool {
        self.labels.contains(label)
    }

    pub(crate) fn in_loop(&self) -> bool {
        self.flags.in_loop
    }

    pub(crate) fn in_switch(&self) -> bool {
        self.flags.in_switch
    }

    pub(crate) fn in_formal_parameters(&self) -> bool {
        self.flags.in_formal_parameters
    }

    pub(crate) fn in_function(&self) -> bool {
        self.flags.in_function
    }

    pub(crate) fn in_match_expression(&self) -> bool {
        self.flags.in_match_expression
    }

    pub(crate) fn in_match_statement(&self) -> bool {
        self.flags.in_match_statement
    }

    pub(crate) fn allow_yield(&self) -> bool {
        self.flags.allow_yield
    }

    pub(crate) fn allow_await(&self) -> bool {
        self.flags.allow_await
    }

    pub(crate) fn allow_directive(&self) -> bool {
        self.flags.allow_directive
    }

    pub(crate) fn allow_super(&self) -> AllowedSuper {
        self.flags.allow_super
    }

    pub(crate) fn in_ambient_context(&self) -> bool {
        self.flags.in_ambient_context
    }

    pub(crate) fn has_simple_parameters(&self) -> bool {
        self.flags.has_simple_parameters
    }

    pub(crate) fn no_in(&self) -> bool {
        self.flags.no_in
    }

    pub(crate) fn no_call(&self) -> bool {
        self.flags.no_call
    }

    pub(crate) fn no_let(&self) -> bool {
        self.flags.no_let
    }

    pub(crate) fn no_anon_function_type(&self) -> bool {
        self.flags.no_anon_function_type
    }

    pub(crate) fn no_conditional_type(&self) -> bool {
        self.flags.no_conditional_type
    }

    pub(crate) fn no_new(&self) -> bool {
        self.flags.no_new
    }

    pub(crate) fn no_record(&self) -> bool {
        self.flags.no_record
    }

    pub(crate) fn errors(self) -> Vec<(Loc, ParseError)> {
        self.errors
    }

    pub(crate) fn parse_options(&self) -> &ParseOptions {
        &self.parse_options
    }

    pub(crate) fn should_parse_types(&self) -> bool {
        self.parse_options.types
    }

    pub fn is_d_ts(&self) -> bool {
        match self.source() {
            Some(file_key) => {
                file_key.check_suffix(".d.ts")
                    || file_key.check_suffix(".d.mts")
                    || file_key.check_suffix(".d.cts")
            }
            None => false,
        }
    }

    pub(crate) fn source(&self) -> Option<FileKey> {
        self.lex_env.source()
    }

    // mutators

    pub(crate) fn error_at(&mut self, loc: Loc, error: ParseError) -> Result<(), Rollback> {
        if let Some(callback) = &self.error_callback {
            callback(error.clone())?;
        }
        self.errors.push((loc, error));
        Ok(())
    }

    /* Since private fields out of scope are a parse error, we keep track of the declared and used
     * private fields.
     *
     * Whenever we enter a class, we push new empty lists of declared and used privates.
     * When we encounter a new declared private, we add it to the top of the declared_privates list
     * via add_declared_private. We do the same with used_privates via add_used_private.
     *
     * When we exit a class, we look for all the unbound private variables. Since class fields
     * are hoisted to the scope of the class, we may need to look further before we conclude that
     * a field is out of scope. To do that, we add all of the unbound private fields to the
     * next used_private list. Once we run out of declared private lists, any leftover used_privates
     * are unbound private variables. */
    pub(crate) fn enter_class(&mut self) {
        self.privates.push((HashSet::new(), Vec::new()));
    }

    pub(crate) fn exit_class(&mut self) -> Result<(), Rollback> {
        fn get_unbound_privates(
            declared_privates: HashSet<String>,
            used_privates: Vec<(String, Loc)>,
        ) -> Vec<(String, Loc)> {
            used_privates
                .into_iter()
                .filter(|(name, _)| !declared_privates.contains(name))
                .collect()
        }

        match self.privates.len() {
            1 => {
                let (declared_privates, used_privates) = self.privates.pop().unwrap();
                let unbound_privates = get_unbound_privates(declared_privates, used_privates);
                for (name, loc) in unbound_privates {
                    self.error_at(loc, ParseError::UnboundPrivate(name))?;
                }
            }
            n if n >= 2 => {
                let (loc_declared_privates, loc_used_privates) = self.privates.pop().unwrap();
                let unbound_privates =
                    get_unbound_privates(loc_declared_privates, loc_used_privates);
                let (decl_head, mut used_head) = self.privates.pop().unwrap();
                used_head.extend(unbound_privates);
                self.privates.push((decl_head, used_head));
            }
            _ => panic!("Internal Error: `exit_class` called before a matching `enter_class`"),
        }
        Ok(())
    }

    pub fn add_declared_private(&mut self, name: String) {
        let (declared, _) = self
            .privates
            .last_mut()
            .expect("Internal Error: Tried to add_declared_private with outside of class scope.");
        declared.insert(name);
    }

    pub(crate) fn add_used_private(&mut self, name: String, loc: Loc) -> Result<(), Rollback> {
        match self.privates.last_mut() {
            Some((_, used)) => used.push((name, loc)),
            None => self.error_at(loc, ParseError::PrivateNotInClass)?,
        }
        Ok(())
    }

    pub(crate) fn consume_comments_until(&mut self, pos: Position) {
        self.consumed_comments_pos = pos;
    }

    // lookaheads

    pub(crate) fn lookahead_0(&mut self) -> &LexResult {
        self.lookahead.peek_0()
    }

    pub(crate) fn lookahead_1(&mut self) -> &LexResult {
        self.lookahead.peek_1()
    }

    pub(crate) fn lookahead_i(&mut self, i: usize) -> &LexResult {
        match i {
            0 => self.lookahead_0(),
            1 => self.lookahead_1(),
            _ => panic!("Invalid lookahead index"),
        }
    }

    // functional operations
    pub(crate) fn with_strict<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        in_strict_mode: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_strict_mode = in_strict_mode;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_in_formal_parameters<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        in_formal_parameters: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_formal_parameters = in_formal_parameters;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_in_match_expression<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        in_match_expression: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_match_expression = in_match_expression;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_in_match_statement<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        in_match_statement: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_match_statement = in_match_statement;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_allow_yield<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        allow_yield: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.allow_yield = allow_yield;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_allow_await<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        allow_await: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.allow_await = allow_await;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_allow_directive<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        allow_directive: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.allow_directive = allow_directive;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_allow_super<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        allow_super: AllowedSuper,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.allow_super = allow_super;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub fn with_ambient_context<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        in_ambient_context: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_ambient_context = in_ambient_context;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_no_let<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        no_let: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.no_let = no_let;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_in_loop<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        in_loop: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_loop = in_loop;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_no_in<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        no_in: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.no_in = no_in;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_no_anon_function_type<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        no_anon_function_type: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.no_anon_function_type = no_anon_function_type;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_no_conditional_type<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        no_conditional_type: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.no_conditional_type = no_conditional_type;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_no_new<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        no_new: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.no_new = no_new;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_no_record<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        no_record: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.no_record = no_record;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_in_switch<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        in_switch: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_switch = in_switch;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_in_export<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        in_export: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_export = in_export;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_in_export_default<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        in_export_default: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_export_default = in_export_default;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_no_call<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        no_call: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.no_call = no_call;
        let result = f(self);
        self.flags = saved_flags;
        result
    }

    pub(crate) fn with_error_callback<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        error_callback: fn(ParseError) -> Result<(), Rollback>,
        f: F,
    ) -> T {
        let saved_error_callback = self.error_callback;
        self.error_callback = Some(error_callback);
        let result = f(self);
        self.error_callback = saved_error_callback;
        result
    }

    pub(crate) fn without_error_callback(&mut self) {
        self.error_callback = None
    }

    /// other helpful functions
    pub(crate) fn add_errors(&mut self, errors: Vec<(Loc, ParseError)>) -> Result<(), Rollback> {
        for (loc, error) in errors {
            self.error_at(loc, error)?;
        }
        Ok(())
    }

    pub(crate) fn last_loc(&self) -> Option<&Loc> {
        self.last_lex_result.as_ref().map(|(loc, _)| loc)
    }

    pub(crate) fn last_token(&self) -> Option<&TokenKind> {
        self.last_lex_result
            .as_ref()
            .map(|(_, token_kind)| token_kind)
    }

    pub(crate) fn with_added_label<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        label: FlowSmolStr,
        f: F,
    ) -> T {
        self.labels.insert(label.clone());
        let result = f(self);
        self.labels.remove(&label);
        result
    }

    pub(crate) fn enter_function<T, F: FnOnce(&mut ParserEnv<'a>) -> T>(
        &mut self,
        async_fn: bool,
        generator: bool,
        simple_params: bool,
        f: F,
    ) -> T {
        let saved_flags = self.flags;
        self.flags.in_formal_parameters = false;
        self.flags.has_simple_parameters = simple_params;
        self.flags.in_function = true;
        self.flags.in_loop = false;
        self.flags.in_switch = false;
        self.flags.in_match_expression = false;
        self.flags.in_match_statement = false;
        self.flags.in_export = false;
        self.flags.in_export_default = false;
        let saved_labels = std::mem::take(&mut self.labels);
        self.flags.allow_await = async_fn;
        self.flags.allow_yield = generator;
        let result = f(self);
        self.flags = saved_flags;
        self.labels = saved_labels;
        result
    }
}

/// IdentifierNames that can't be used as Identifiers in strict mode.
///
/// https://tc39.es/ecma262/#sec-strict-mode-of-ecmascript
pub(crate) fn is_strict_reserved(s: &str) -> bool {
    match s {
        "implements" | "interface" | "let" | "package" | "private" | "protected" | "public"
        | "static" | "yield" => true,
        _ => false,
    }
}

/// Tokens which, if parsed as an identifier, are reserved words in strict mode.
pub(crate) fn token_is_strict_reserved(token: &TokenKind) -> bool {
    match token {
        TokenKind::TIdentifier { value, .. } => is_strict_reserved(value),
        TokenKind::TInterface
        | TokenKind::TImplements
        | TokenKind::TLet
        | TokenKind::TPackage
        | TokenKind::TPrivate
        | TokenKind::TProtected
        | TokenKind::TPublic
        | TokenKind::TStatic
        | TokenKind::TYield => true,
        _ => false,
    }
}

// #sec-strict-mode-of-ecmascript
pub(crate) fn is_restricted(s: &str) -> bool {
    match s {
        "eval" | "arguments" => true,
        _ => false,
    }
}

/// Words that are sometimes reserved, and sometimes allowed as identifiers
/// (namely "await" and "yield")
///
/// https://tc39.es/ecma262/#sec-keywords-and-reserved-words
pub(crate) fn is_contextually_reserved(str_val: &str) -> bool {
    match str_val {
        "await" | "yield" => true,
        _ => false,
    }
}

/// Words that are always reserved (mostly keywords)
///
/// https://tc39.es/ecma262/#sec-keywords-and-reserved-words
pub(crate) fn is_reserved(str_val: &str) -> bool {
    match str_val {
        "break" | "case" | "catch" | "class" | "const" | "continue" | "debugger" | "default"
        | "delete" | "do" | "else" | "enum" | "export" | "extends" | "false" | "finally"
        | "for" | "function" | "if" | "import" | "in" | "instanceof" | "new" | "null"
        | "return" | "super" | "switch" | "this" | "throw" | "true" | "try" | "typeof" | "var"
        | "void" | "while" | "with" => true,
        _ => false,
    }
}

pub(crate) fn is_reserved_type(str_val: &str) -> bool {
    match str_val {
        "any" | "bigint" | "bool" | "boolean" | "const" | "empty" | "extends" | "false"
        | "function" | "interface" | "keyof" | "mixed" | "never" | "null" | "number"
        | "readonly" | "static" | "string" | "symbol" | "true" | "typeof" | "undefined"
        | "unknown" | "void" => true,
        _ => false,
    }
}

pub(crate) fn token_is_reserved_type(t: &TokenKind) -> bool {
    match t {
        TokenKind::TIdentifier { raw, .. } if is_reserved_type(raw) => true,
        TokenKind::TAnyType
        | TokenKind::TBigintType
        | TokenKind::TBooleanType(_)
        | TokenKind::TConst
        | TokenKind::TEmptyType
        | TokenKind::TExtends
        | TokenKind::TFalse
        | TokenKind::TFunction
        | TokenKind::TInterface
        | TokenKind::TKeyof
        | TokenKind::TMixedType
        | TokenKind::TNeverType
        | TokenKind::TNull
        | TokenKind::TNumberType
        | TokenKind::TReadonly
        | TokenKind::TWriteonly
        | TokenKind::TStatic
        | TokenKind::TStringType
        | TokenKind::TSymbolType
        | TokenKind::TTrue
        | TokenKind::TTypeof
        | TokenKind::TUndefinedType
        | TokenKind::TUnknownType
        | TokenKind::TVoidType => true,
        _ => false,
    }
}

fn token_is_type_identifier_under_lex_mode(lex_mode: LexMode, t: &TokenKind) -> bool {
    match lex_mode {
        LexMode::Type => match t {
            TokenKind::TIdentifier { .. } => true,
            _ => false,
        },
        LexMode::Normal => {
            /* Sometimes we peek at type identifiers while in normal lex mode. For
            example, when deciding whether a `type` token is an identifier or the
            start of a type declaration, based on whether the following token
            `is_type_identifier`. */
            match t {
                TokenKind::TIdentifier { raw, .. } if is_reserved_type(raw) => false,
                /* reserved type identifiers, but these don't appear in NORMAL mode */
                TokenKind::TAnyType | TokenKind::TMixedType | TokenKind::TEmptyType | TokenKind::TNumberType
                | TokenKind::TBigintType | TokenKind::TStringType | TokenKind::TVoidType | TokenKind::TSymbolType
                | TokenKind::TUnknownType | TokenKind::TNeverType | TokenKind::TUndefinedType
                | TokenKind::TBooleanType(_) | TokenKind::TNumberSingletonType { .. } | TokenKind::TBigintSingletonType { .. }
                /* identifier-ish */
                | TokenKind::TAsync | TokenKind::TAwait | TokenKind::TBreak | TokenKind::TCase | TokenKind::TCatch
                | TokenKind::TClass | TokenKind::TConst | TokenKind::TContinue | TokenKind::TDebugger | TokenKind::TDeclare
                | TokenKind::TDefault | TokenKind::TDelete | TokenKind::TDo | TokenKind::TElse | TokenKind::TEnum
                | TokenKind::TExport | TokenKind::TExtends | TokenKind::TFalse | TokenKind::TFinally | TokenKind::TFor
                | TokenKind::TIdentifier { .. } | TokenKind::TIf | TokenKind::TImplements | TokenKind::TImport | TokenKind::TIn
                | TokenKind::TInstanceof | TokenKind::TInterface | TokenKind::TLet | TokenKind::TMatch | TokenKind::TRecord | TokenKind::TNew
                | TokenKind::TNull | TokenKind::TOf | TokenKind::TOpaque | TokenKind::TPackage | TokenKind::TPrivate
                | TokenKind::TProtected | TokenKind::TPublic | TokenKind::TReturn | TokenKind::TSuper | TokenKind::TSwitch
                | TokenKind::TThis | TokenKind::TThrow | TokenKind::TTrue | TokenKind::TTry | TokenKind::TType | TokenKind::TVar
                | TokenKind::TWhile | TokenKind::TWith | TokenKind::TYield => true,
                /* identifier-ish, but not valid types */
                TokenKind::TStatic | TokenKind::TTypeof | TokenKind::TFunction | TokenKind::TKeyof | TokenKind::TReadonly
                | TokenKind::TWriteonly | TokenKind::TInfer | TokenKind::TIs | TokenKind::TAsserts | TokenKind::TImplies | TokenKind::TVoid
                | TokenKind::TRendersQuestion | TokenKind::TRendersStar => false,
                /* syntax */
                TokenKind::TLcurly | TokenKind::TRcurly | TokenKind::TLcurlybar | TokenKind::TRcurlybar | TokenKind::TLparen
                | TokenKind::TRparen | TokenKind::TLbracket | TokenKind::TRbracket | TokenKind::TSemicolon | TokenKind::TComma
                | TokenKind::TPeriod | TokenKind::TArrow | TokenKind::TEllipsis | TokenKind::TAt | TokenKind::TPound
                | TokenKind::TChecks | TokenKind::TRshift3Assign | TokenKind::TRshiftAssign | TokenKind::TLshiftAssign
                | TokenKind::TBitXorAssign | TokenKind::TBitOrAssign | TokenKind::TBitAndAssign | TokenKind::TModAssign
                | TokenKind::TDivAssign | TokenKind::TMultAssign | TokenKind::TExpAssign | TokenKind::TMinusAssign
                | TokenKind::TPlusAssign | TokenKind::TNullishAssign | TokenKind::TAndAssign | TokenKind::TOrAssign
                | TokenKind::TAssign | TokenKind::TPlingPeriod | TokenKind::TPlingPling | TokenKind::TPling | TokenKind::TColon
                | TokenKind::TOr | TokenKind::TAnd | TokenKind::TBitOr | TokenKind::TBitXor | TokenKind::TBitAnd | TokenKind::TEqual
                | TokenKind::TNotEqual | TokenKind::TStrictEqual | TokenKind::TStrictNotEqual | TokenKind::TLessThanEqual
                | TokenKind::TGreaterThanEqual | TokenKind::TLessThan | TokenKind::TGreaterThan | TokenKind::TLshift
                | TokenKind::TRshift | TokenKind::TRshift3 | TokenKind::TPlus | TokenKind::TMinus | TokenKind::TDiv
                | TokenKind::TMult | TokenKind::TExp | TokenKind::TMod | TokenKind::TNot | TokenKind::TBitNot | TokenKind::TIncr
                | TokenKind::TDecr | TokenKind::TInterpreter(_, _) | TokenKind::TEof => false,
                /* literals */
                TokenKind::TNumber { .. } | TokenKind::TBigint { .. } | TokenKind::TString(_, _, _, _) | TokenKind::TTemplatePart(_)
                | TokenKind::TRegexp(_, _, _) => false,
                /* misc that shouldn't appear in NORMAL mode */
                TokenKind::TJsxIdentifier { .. } | TokenKind::TJsxChildText(_, _, _) | TokenKind::TJsxQuoteText(_, _, _)
                | TokenKind::TError(_) => false,
            }
        }
        LexMode::JsxTag | LexMode::JsxChild | LexMode::Regexp => false,
    }
}

pub(crate) fn token_is_type_identifier(env: &ParserEnv, t: &TokenKind) -> bool {
    token_is_type_identifier_under_lex_mode(env.lex_mode(), t)
}

pub(crate) fn token_is_variance(token: &TokenKind) -> bool {
    match token {
        TokenKind::TPlus | TokenKind::TMinus => true,
        _ => false,
    }
}

/// Answer questions about what comes next
pub(crate) mod peek {
    use super::*;
    use crate::lex_env::LexErrors;
    use crate::token::TokenKind;

    pub(crate) fn ith_token<'a>(env: &'a mut ParserEnv, i: usize) -> &'a TokenKind {
        &env.lookahead_i(i).token_kind
    }

    pub(crate) fn ith_loc<'a>(env: &'a mut ParserEnv, i: usize) -> &'a Loc {
        &env.lookahead_i(i).loc
    }

    pub(crate) fn ith_errors<'a>(env: &'a mut ParserEnv, i: usize) -> &'a LexErrors {
        &env.lookahead_i(i).errors
    }

    pub(crate) fn ith_comments(env: &mut ParserEnv, i: usize) -> Vec<Comment<Loc>> {
        let consumed_comments_pos = env.consumed_comments_pos;
        let comments = &env.lookahead_i(i).comments;
        comments
            .iter()
            .filter(|comment| comment.loc.start >= consumed_comments_pos)
            .cloned()
            .collect()
    }

    pub(crate) fn token<'a>(env: &'a mut ParserEnv) -> &'a TokenKind {
        ith_token(env, 0)
    }

    pub(crate) fn loc<'a>(env: &'a mut ParserEnv) -> &'a Loc {
        ith_loc(env, 0)
    }

    /// loc_skip_lookahead is used to give a loc hint to optional tokens such as type annotations
    pub(crate) fn loc_skip_lookahead(env: &ParserEnv) -> Loc {
        let loc = env
            .last_loc()
            .expect("Peeking current location when not available");
        Loc {
            start: loc.end,
            ..loc.dupe()
        }
    }

    pub(crate) fn errors(env: &mut ParserEnv) -> Vec<(Loc, ParseError)> {
        ith_errors(env, 0).as_errors().to_vec()
    }

    pub(crate) fn comments(env: &mut ParserEnv) -> Vec<Comment<Loc>> {
        ith_comments(env, 0)
    }

    pub(crate) fn has_eaten_comments(env: &mut ParserEnv) -> bool {
        let consumed_comments_pos = env.consumed_comments_pos;
        let comments = &env.lookahead_0().comments;
        comments
            .iter()
            .any(|comment| comment.loc.start < consumed_comments_pos)
    }

    pub(crate) fn lex_env<'a>(env: &mut ParserEnv<'a>) -> wrapped_lex_env::WrappedLexEnv<'a> {
        env.lookahead.lex_env_0().clone()
    }

    /// True if there is a line terminator before the next token
    pub(crate) fn ith_is_line_terminator(env: &mut ParserEnv, i: usize) -> bool {
        let loc = if i > 0 {
            Some(ith_loc(env, i - 1))
        } else {
            env.last_loc()
        };
        match loc {
            None => false,
            Some(loc_prev) => {
                let loc_prev_start_line = loc_prev.start.line;
                ith_loc(env, i).start.line > loc_prev_start_line
            }
        }
    }

    pub(crate) fn is_line_terminator(env: &mut ParserEnv) -> bool {
        ith_is_line_terminator(env, 0)
    }

    pub(crate) fn ith_is_implicit_semicolon(env: &mut ParserEnv, i: usize) -> bool {
        match ith_token(env, i) {
            TokenKind::TEof | TokenKind::TRcurly => true,
            TokenKind::TSemicolon => false,
            _ => ith_is_line_terminator(env, i),
        }
    }

    pub(crate) fn is_implicit_semicolon(env: &mut ParserEnv) -> bool {
        ith_is_implicit_semicolon(env, 0)
    }

    pub(crate) fn token_kind_is_identifier(t: &TokenKind) -> bool {
        match t {
            t if token_is_strict_reserved(t) => true,
            TokenKind::TType
            | TokenKind::TOpaque
            | TokenKind::TOf
            | TokenKind::TDeclare
            | TokenKind::TAsync
            | TokenKind::TAwait
            | TokenKind::TEnum
            | TokenKind::TMatch
            | TokenKind::TRecord
            | TokenKind::TPound
            | TokenKind::TIdentifier { .. }
            | TokenKind::TReadonly
            | TokenKind::TWriteonly
            | TokenKind::TInfer => true,
            _ => false,
        }
    }

    pub(crate) fn ith_is_identifier(env: &mut ParserEnv, i: usize) -> bool {
        let t = ith_token(env, i);
        token_kind_is_identifier(t)
    }

    pub(crate) fn ith_is_type_identifier(env: &mut ParserEnv, i: usize) -> bool {
        let lev_mode = env.lex_mode();
        token_is_type_identifier_under_lex_mode(lev_mode, ith_token(env, i))
    }

    pub(crate) fn ith_is_identifier_name(env: &mut ParserEnv, i: usize) -> bool {
        ith_is_identifier(env, i) || ith_is_type_identifier(env, i)
    }

    // let ith_is_object_key ~i ~is_class env =
    //   match ith_token ~i env with
    //   | T_STRING _ | T_NUMBER _ | T_BIGINT _ | T_LBRACKET -> true
    //   | T_POUND when is_class -> true
    //   | _ -> ith_is_identifier_name ~i env
    pub(crate) fn ith_is_object_key(env: &mut ParserEnv, i: usize, is_class: bool) -> bool {
        match ith_token(env, i) {
            TokenKind::TString { .. }
            | TokenKind::TNumber { .. }
            | TokenKind::TBigint { .. }
            | TokenKind::TLbracket => true,
            TokenKind::TPound if is_class => true,
            _ => ith_is_identifier_name(env, i),
        }
    }

    /// This returns true if the next token is identifier-ish (even if it is an error)
    pub(crate) fn is_identifier(env: &mut ParserEnv) -> bool {
        ith_is_identifier(env, 0)
    }

    pub(crate) fn is_function(env: &mut ParserEnv) -> bool {
        token(env) == &TokenKind::TFunction
            || (token(env) == &TokenKind::TAsync
                && ith_token(env, 1) == &TokenKind::TFunction
                && loc(env).end.line == ith_loc(env, 1).start.line)
    }

    pub(crate) fn is_hook(env: &mut ParserEnv) -> bool {
        match token(env) {
            TokenKind::TIdentifier { raw, .. } if raw == "hook" => {
                env.parse_options().components
                    && ith_is_identifier(env, 1)
                    && loc(env).end.line == ith_loc(env, 1).start.line
            }
            TokenKind::TAsync => {
                env.parse_options().components
                    && loc(env).end.line == ith_loc(env, 1).start.line
                    && matches!(
                        ith_token(env, 1),
                        TokenKind::TIdentifier { raw, .. } if raw == "hook"
                    )
            }
            _ => false,
        }
    }

    pub(crate) fn is_class(env: &mut ParserEnv) -> bool {
        match token(env) {
            TokenKind::TClass | TokenKind::TAt => true,
            TokenKind::TIdentifier { raw, .. } if raw == "abstract" => {
                ith_token(env, 1) == &TokenKind::TClass
            }
            _ => false,
        }
    }

    pub(crate) fn is_component(env: &mut ParserEnv) -> bool {
        if !env.parse_options().components {
            return false;
        }
        match token(env) {
            TokenKind::TIdentifier { raw, .. } if raw == "component" => ith_is_identifier(env, 1),
            TokenKind::TAsync => {
                !ith_is_line_terminator(env, 1)
                    && matches!(
                        ith_token(env, 1),
                        TokenKind::TIdentifier { raw, .. } if raw == "component"
                    )
            }
            _ => false,
        }
    }

    pub(crate) fn is_record(env: &mut ParserEnv) -> bool {
        env.parse_options().records
            && token(env) == &TokenKind::TRecord
            && !ith_is_line_terminator(env, 1)
            && ith_is_identifier(env, 1)
    }

    pub(crate) fn is_renders_ident(env: &mut ParserEnv) -> bool {
        match token(env) {
            TokenKind::TIdentifier { raw, .. } if raw == "renders" => true,
            _ => false,
        }
    }
}

/******************************************************************************/
/* Errors                                                                     */
/******************************************************************************/

impl<'a> ParserEnv<'a> {
    /// Complains about an error at the location of the lookahead  
    pub(crate) fn error(&mut self, e: ParseError) -> Result<(), Rollback> {
        let loc = peek::loc(self).dupe();
        self.error_at(loc, e)
    }

    fn get_unexpected_error(expected: Option<String>, token: &TokenKind) -> ParseError {
        let unexpected = TokenKind::explanation_of_token(token);
        match expected {
            Some(expected_msg) => ParseError::UnexpectedWithExpected(unexpected, expected_msg),
            None => ParseError::Unexpected(unexpected),
        }
    }

    pub(crate) fn error_unexpected(&mut self, expected: Option<String>) -> Result<(), Rollback> {
        /* So normally we consume the lookahead lex result when Eat.token calls
         * Parser_env.advance, which will add any lexing errors to our list of errors.
         * However, raising an unexpected error for a lookahead is kind of like
         * consuming that token, so we should process any lexing errors before
         * complaining about the unexpected token */
        for (loc, e) in peek::errors(self) {
            self.error_at(loc, e)?;
        }
        let error = Self::get_unexpected_error(expected, peek::token(self));
        self.error(error)?;
        Ok(())
    }

    pub(crate) fn error_on_decorators(
        &mut self,
        decorators: &[class::Decorator<Loc, Loc>],
    ) -> Result<(), Rollback> {
        for d in decorators {
            self.error_at(d.loc.dupe(), ParseError::UnsupportedDecorator)?;
        }
        Ok(())
    }

    pub(crate) fn error_nameless_declaration(&mut self, kind: &str) -> Result<(), Rollback> {
        let expected = if self.in_export() {
            format!(
                "an identifier. When exporting a {} as a named export, you must specify a {} name. Did you mean `export default {} ...`?",
                kind, kind, kind
            )
        } else {
            "an identifier".to_string()
        };
        self.error_unexpected(Some(expected))?;
        Ok(())
    }

    pub(crate) fn strict_error(&mut self, e: ParseError) -> Result<(), Rollback> {
        if self.in_strict_mode() {
            self.error(e)?;
        }
        Ok(())
    }

    pub(crate) fn strict_error_at(
        &mut self,
        (loc, error): (Loc, ParseError),
    ) -> Result<(), Rollback> {
        if self.in_strict_mode() {
            self.error_at(loc, error)?;
        }
        Ok(())
    }

    pub(crate) fn function_as_statement_error_at(&mut self, loc: Loc) -> Result<(), Rollback> {
        self.error_at(
            loc,
            ParseError::FunctionAsStatement {
                in_strict_mode: self.in_strict_mode(),
            },
        )?;
        Ok(())
    }
}

/// Consume zero or more tokens
pub mod eat {
    use super::*;

    /// Consume a single token
    pub(crate) fn token(env: &mut ParserEnv) -> Result<(), Rollback> {
        /* If there's a token_sink, emit the lexed token before moving forward.
         * Skip T_EOF — upstream Hermes' token stream does not include a
         * sentinel end-of-input token, so neither should ours. The serializer
         * maps T_EOF to Punctuator with empty value
         * (`token_kind_to_estree_type_index` in serializer.rs), and the adapter
         * used to pop it in JS; emit-time skip is the cleaner fix. */
        let token_sink = env.token_sink.take();
        match token_sink {
            None => {}
            Some(token_sink) if matches!(peek::token(env), TokenKind::TEof) => {
                env.token_sink = Some(token_sink);
            }
            Some(token_sink) => {
                let token_loc = peek::loc(env).dupe();
                let token = peek::token(env).clone();
                let token_loc = match &token {
                    TokenKind::TInterpreter(loc, _) => loc.dupe(),
                    _ => token_loc,
                };
                let token_sink = send_token_to_sink(
                    token_sink,
                    TokenSinkResult {
                        token_loc,
                        token_kind: token,
                        /*
                         * The lex mode is useful because it gives context to some
                         * context-sensitive tokens.
                         *
                         * Some examples of such tokens include:
                         *
                         * `=>` - Part of an arrow function? or part of a type annotation?
                         * `<`  - A less-than? Or an opening to a JSX element?
                         * ...etc...
                         */
                        token_context: env.lex_mode(),
                    },
                );
                env.token_sink = Some(token_sink);
            }
        }

        env.lex_env = peek::lex_env(env);

        for (loc, e) in peek::errors(env) {
            env.error_at(loc, e)?;
        }
        let new_comments = env.lookahead_0().comments.clone();
        env.comments.extend(new_comments);
        env.last_lex_result = {
            let result = env.lookahead_0();
            Some((result.loc.dupe(), result.token_kind.clone()))
        };

        env.lookahead.junk();
        Ok(())
    }

    /* [maybe env t] eats the next token and returns [true] if it is [t], else return [false] */
    pub(crate) fn maybe(env: &mut ParserEnv, t: TokenKind) -> Result<bool, Rollback> {
        let is_t = peek::token(env) == &t;
        if is_t {
            token(env)?;
        }
        Ok(is_t)
    }

    pub(crate) fn push_lex_mode(env: &mut ParserEnv, mode: LexMode) {
        env.lex_mode_stack.push(mode);
        let new_lex_mode = env.lex_mode();
        env.lookahead = lookahead::Lookahead::new(new_lex_mode, &env.lex_env);
    }

    pub(crate) fn pop_lex_mode(env: &mut ParserEnv) {
        env.lex_mode_stack
            .pop()
            .expect("Popping lex mode from empty stack");
        let new_lex_mode = env.lex_mode();
        env.lookahead = lookahead::Lookahead::new(new_lex_mode, &env.lex_env);
    }

    pub(crate) fn double_pop_lex_mode(env: &mut ParserEnv) {
        env.lex_mode_stack
            .pop()
            .expect("Popping lex mode from empty stack");
        env.lex_mode_stack
            .pop()
            .expect("Popping lex mode from empty stack");
        let new_lex_mode = env.lex_mode();
        env.lookahead = lookahead::Lookahead::new(new_lex_mode, &env.lex_env);
    }

    pub(crate) fn rescan_as_template_from<'a>(
        env: &mut ParserEnv<'a>,
        prev_lex_env: wrapped_lex_env::WrappedLexEnv<'a>,
    ) {
        env.lookahead.rescan_template_part_from(&prev_lex_env);
    }

    pub(crate) fn trailing_comments(env: &mut ParserEnv) -> Vec<Comment<Loc>> {
        let loc = peek::loc(env).dupe();
        if peek::token(env) == &TokenKind::TComma && peek::ith_is_line_terminator(env, 1) {
            let trailing_before_comma = peek::comments(env);
            let trailing_after_comma: Vec<Comment<Loc>> = env
                .lookahead_i(1)
                .comments
                .clone()
                .into_iter()
                .filter(|comment| comment.loc.start.line <= loc.end.line)
                .collect();
            let mut trailing = trailing_before_comma;
            trailing.extend(trailing_after_comma);
            env.consume_comments_until(Position {
                line: loc.end.line + 1,
                column: 0,
            });
            trailing
        } else {
            let trailing = peek::comments(env);
            env.consume_comments_until(loc.end);
            trailing
        }
    }

    pub(crate) fn comments_until_next_line(env: &mut ParserEnv) -> Vec<Comment<Loc>> {
        if let Some(last_loc) = env.last_loc() {
            let last_loc_end = last_loc.end;
            let comments: Vec<Comment<Loc>> = peek::comments(env)
                .into_iter()
                .filter(|comment| comment.loc.start.line <= last_loc_end.line)
                .collect();
            env.consume_comments_until(Position {
                line: last_loc_end.line + 1,
                column: 0,
            });
            comments
        } else {
            Vec::new()
        }
    }

    pub(crate) fn program_comments(env: &mut ParserEnv) -> Vec<Comment<Loc>> {
        let comments = peek::comments(env);

        for (i, comment) in comments.iter().enumerate().rev() {
            if comment.text.contains("@flow") {
                env.consumed_comments_pos = comment.loc.end;
                return comments[..=i].to_vec();
            }
        }

        /* If there is no @flow directive, consider the first block comment a program comment if
        it starts with '/' '*' '*' */
        match comments.first() {
            Some(comment)
                if matches!(comment.kind, CommentKind::Block) && comment.text.starts_with('*') =>
            {
                env.consumed_comments_pos = comment.loc.end;
                vec![comment.clone()]
            }
            _ => Vec::new(),
        }
    }
}

pub mod expect {
    use super::*;

    pub(crate) fn get_error(env: &mut ParserEnv, t: &TokenKind) -> (Loc, ParseError) {
        let expected = TokenKind::explanation_of_token_with_article(t);
        (
            peek::loc(env).clone(),
            ParserEnv::get_unexpected_error(Some(expected), peek::token(env)),
        )
    }

    pub(crate) fn error(env: &mut ParserEnv, t: &TokenKind) -> Result<(), Rollback> {
        let expected = TokenKind::explanation_of_token_with_article(t);
        env.error_unexpected(Some(expected))?;
        Ok(())
    }

    pub(crate) fn token(env: &mut ParserEnv, t: TokenKind) -> Result<(), Rollback> {
        if peek::token(env) != &t {
            error(env, &t)?;
        }
        eat::token(env)?;
        Ok(())
    }

    /// [token_maybe env T_FOO] eats a token if it is [T_FOO], and errors without consuming if
    /// not. Returns whether it consumed a token, like [Eat.maybe].  
    pub(crate) fn token_maybe(env: &mut ParserEnv, t: TokenKind) -> Result<bool, Rollback> {
        let ate = eat::maybe(env, t.clone())?;
        if !ate {
            error(env, &t)?;
        }
        Ok(ate)
    }

    /// [token_opt env T_FOO] eats a token if it is [T_FOO], and errors without consuming if not.
    /// This differs from [token], which always consumes. Only use [token_opt] when it's ok for
    /// the parser to not advance, like if you are guaranteed that something else has eaten a
    /// token.
    pub(crate) fn token_opt(env: &mut ParserEnv, t: TokenKind) -> Result<(), Rollback> {
        token_maybe(env, t)?;
        Ok(())
    }

    pub(crate) fn identifier(env: &mut ParserEnv, name: &str) -> Result<(), Rollback> {
        let t = peek::token(env);
        match t {
            TokenKind::TIdentifier { raw, .. } if raw == name => {}
            _ => {
                let expected = format!("the identifier `{}`", name);
                ParserEnv::error_unexpected(env, Some(expected))?;
            }
        }
        eat::token(env)?;
        Ok(())
    }
}

/// This module allows you to try parsing and rollback if you need. This is not
/// cheap and its usage is strongly discouraged
pub mod try_parse {
    use super::*;

    #[derive(Debug)]
    pub enum ParseResult<T> {
        ParsedSuccessfully(T),
        FailedToParse,
    }

    pub struct Rollback;

    struct SavedState<'a> {
        saved_errors: Vec<(Loc, ParseError)>,
        saved_comments: Vec<Comment<Loc>>,
        saved_last_lex_result: Option<(Loc, TokenKind)>,
        saved_lex_mode_stack: Vec<LexMode>,
        saved_lex_env: wrapped_lex_env::WrappedLexEnv<'a>,
        saved_consumed_comments_pos: Position,
        token_buffer: bool,
    }

    fn save_state<'a>(env: &mut ParserEnv<'a>) -> SavedState<'a> {
        let token_buffer = match env.token_sink.take() {
            None => false,
            Some(orig_token_sink) => {
                env.token_sink = Some(TokenSink::Buffered {
                    next: Box::new(orig_token_sink),
                    buffer: VecDeque::new(),
                });
                true
            }
        };

        SavedState {
            saved_errors: env.errors.clone(),
            saved_comments: env.comments.clone(),
            saved_last_lex_result: env.last_lex_result.clone(),
            saved_lex_mode_stack: env.lex_mode_stack.clone(),
            saved_lex_env: env.lex_env.clone(),
            saved_consumed_comments_pos: env.consumed_comments_pos,
            token_buffer,
        }
    }

    fn reset_token_sink<'a>(env: &mut ParserEnv<'a>, token_buffer_info: bool, flush: bool) {
        if token_buffer_info {
            let token_sink = env.token_sink.take();
            match token_sink {
                Some(TokenSink::Buffered { next, buffer }) => {
                    let mut next = *next;
                    if flush {
                        for token_data in buffer {
                            next = send_token_to_sink(next, token_data);
                        }
                    }
                    env.token_sink = Some(next);
                }
                None | Some(TokenSink::Direct(_)) => unreachable!(),
            }
        }
    }

    fn rollback_state<'a, T>(
        env: &mut ParserEnv<'a>,
        saved_state: SavedState<'a>,
    ) -> ParseResult<T> {
        reset_token_sink(env, saved_state.token_buffer, false);
        env.errors = saved_state.saved_errors;
        env.comments = saved_state.saved_comments;
        env.last_lex_result = saved_state.saved_last_lex_result;
        env.lex_mode_stack = saved_state.saved_lex_mode_stack;
        env.lex_env = saved_state.saved_lex_env;
        env.consumed_comments_pos = saved_state.saved_consumed_comments_pos;
        let new_lex_mode = env.lex_mode();
        env.lookahead = lookahead::Lookahead::new(new_lex_mode, &env.lex_env);

        ParseResult::FailedToParse
    }

    fn success<'a, T>(
        env: &mut ParserEnv<'a>,
        saved_state: SavedState<'a>,
        result: T,
    ) -> ParseResult<T> {
        reset_token_sink(env, saved_state.token_buffer, true);
        ParseResult::ParsedSuccessfully(result)
    }

    pub(crate) fn to_parse<'a, T, F>(env: &mut ParserEnv<'a>, parse: F) -> ParseResult<T>
    where
        F: FnOnce(&mut ParserEnv<'a>) -> Result<T, Rollback>,
    {
        let saved_state = save_state(env);
        match parse(env) {
            Ok(result) => success(env, saved_state, result),
            Err(_) => rollback_state(env, saved_state),
        }
    }

    pub(crate) fn or_else<'a, T, F>(env: &mut ParserEnv<'a>, parse: F, fallback: T) -> T
    where
        F: FnOnce(&mut ParserEnv<'a>) -> Result<T, Rollback>,
    {
        match to_parse(env, parse) {
            ParseResult::ParsedSuccessfully(result) => result,
            ParseResult::FailedToParse => fallback,
        }
    }
}
