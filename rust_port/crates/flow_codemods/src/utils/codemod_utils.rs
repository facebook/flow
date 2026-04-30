/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use flow_common::options::Options;
use flow_parser::ast;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::flow_ast_differ;
use flow_parser_utils_output::js_layout_generator::Opts as LayoutOptions;
use flow_parser_utils_output::replacement_printer;
use flow_parser_utils_output::replacement_printer::Patch;
use flow_server_utils::file_input::FileInput;
use flow_services_code_action::code_action_utils;

use super::codemod_ast_mapper::CodemodAstMapper;
use super::codemod_ast_reducer::CodemodAstReducer;
use super::codemod_context;
use super::codemod_report;

fn diff_heaps_set_diff(file_key: &FileKey, patch: &Patch) {
    super::diff_heaps::set_diff(file_key, patch);
}

fn reducer_program<A>(
    reducer: &mut CodemodAstReducer<A>,
    ast: &ast::Program<Loc, Loc>,
) -> ast::Program<Loc, Loc> {
    reducer.map_program(ast)
}

fn mapper_program<A>(
    mapper: &mut CodemodAstMapper<A>,
    ast: &ast::Program<Loc, Loc>,
) -> ast::Program<Loc, Loc> {
    mapper.map_program(ast)
}

fn init_loggers(options: &Options) {
    flow_logging_utils::init_loggers(options, None);
}

fn shared_mem_init(
    _num_workers: usize,
    _shared_mem_config: &(),
) -> Result<flow_heap::parsing_heaps::SharedMem, ()> {
    Ok(flow_heap::parsing_heaps::SharedMem::new())
}

fn make_genv(
    init_id: &str,
    options: &Options,
    handle: flow_heap::parsing_heaps::SharedMem,
) -> flow_server_env::server_env::Genv {
    flow_server::server_env_build::make_genv(
        std::sync::Arc::new(options.clone()),
        init_id,
        std::sync::Arc::new(handle),
    )
}

const ALPHANUMERIC_ALPHABET: &[u8] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

fn short_string_with_alphabet(alphabet: &[u8]) -> String {
    use std::time::SystemTime;
    let seed = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64;
    let mut r = seed;
    let mut cs = Vec::new();
    while r > 0 {
        let c = alphabet[(r as usize) % alphabet.len()];
        cs.push(c as char);
        r >>= 6;
    }
    cs.reverse();
    cs.into_iter().collect()
}

fn random_id_short_string() -> String {
    short_string_with_alphabet(ALPHANUMERIC_ALPHABET)
}

pub enum AbstractCodemodRunner<A, T> {
    Mapper(Box<dyn Fn(T) -> CodemodAstMapper<A> + Send + Sync>),
    Reducer(Box<dyn Fn(T) -> CodemodAstReducer<A> + Send + Sync>),
}

pub enum CodemodRunner<'cx, A> {
    TypedRunner(AbstractCodemodRunner<A, codemod_context::typed::TypedCodemodContext<'cx>>),
    UntypedFlowInitRunner {
        init: Box<dyn Fn(&std::sync::Arc<flow_heap::parsing_heaps::SharedMem>) + Send + Sync>,
        runner: AbstractCodemodRunner<
            A,
            codemod_context::untyped_flow_init::UntypedFlowInitCodemodContext,
        >,
    },
    UntypedRunner(AbstractCodemodRunner<A, codemod_context::untyped::UntypedCodemodContext>),
}

pub struct JobConfig<'cx, A> {
    pub runner: CodemodRunner<'cx, A>,
    pub reporter: codemod_report::CodemodReport<A>,
}

// Mappers produce new ASTs, which are saved to the heap.
fn save_ast_diff(
    opts: &LayoutOptions,
    file_key: &FileKey,
    ast: &ast::Program<Loc, Loc>,
    ast_prime: &ast::Program<Loc, Loc>,
) {
    let diff = flow_ast_differ::program(ast, ast_prime);
    if diff.is_empty() {
        return;
    }
    let file_path = file_key.to_absolute();
    let file_input = FileInput::FileName(file_path);
    let patch = replacement_printer::mk_patch_ast_differ_unsafe(opts, &diff, &file_input);
    diff_heaps_set_diff(file_key, &patch);
}

pub fn make_visitor<A, T>(
    runner: AbstractCodemodRunner<A, T>,
    options: &Options,
    ast: &ast::Program<Loc, Loc>,
    cctx: T,
) -> A {
    let prog_loc = &ast.loc;
    let file = prog_loc.source.as_ref().expect("No source for AST").clone();
    match runner {
        AbstractCodemodRunner::Reducer(reducer_fn) => {
            let mut reducer = reducer_fn(cctx);
            let _program = reducer_program(&mut reducer, ast);
            reducer.acc
        }
        AbstractCodemodRunner::Mapper(mapper_fn) => {
            let mut mapper = mapper_fn(cctx);
            let ast_prime = mapper_program(&mut mapper, ast);
            let opts = LayoutOptions {
                preserve_formatting: true,
                ..code_action_utils::layout_options(options)
            };
            save_ast_diff(&opts, &file, ast, &ast_prime);
            mapper.acc
        }
    }
}

pub fn initialize_logs(options: &Options) {
    init_loggers(options);
}

pub struct MakeMain<Runner: super::codemod_runner::Runnable> {
    _phantom: std::marker::PhantomData<Runner>,
}

impl<Runner: super::codemod_runner::Runnable> MakeMain<Runner> {
    pub fn main(
        options: &Options,
        write: bool,
        repeat: bool,
        log_level: Option<flow_hh_logger::Level>,
        roots: BTreeSet<FileKey>,
    ) {
        let init_id = random_id_short_string();
        initialize_logs(options);
        let log_level = match log_level {
            Some(level) => level,
            None => flow_hh_logger::Level::Off,
        };
        flow_hh_logger::level::set_min_level(log_level);
        let num_workers = options.max_workers as usize;
        let handle = match shared_mem_init(num_workers, &()) {
            Ok(handle) => handle,
            Err(()) => panic!("Out_of_shared_memory"),
        };
        let genv = make_genv(&init_id, options, handle);
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime");
        rt.block_on(Runner::run(&genv, write, repeat, roots));
    }
}
