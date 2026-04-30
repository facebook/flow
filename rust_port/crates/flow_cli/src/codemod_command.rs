/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;

use dupe::Dupe;
use flow_codemods::annotate_exports::AnnotateExportsMapper;
use flow_codemods::annotate_exports::signature_verification_error_stats::SignatureVerificationErrorStats;
use flow_codemods::annotate_literal_declarations::AnnotateLiteralDeclarationsMapper;
use flow_codemods::annotate_literal_declarations::stats::Stats as AnnotateLiteralStats;
use flow_codemods::annotate_optional_properties::AnnotateOptionalPropertiesMapper;
use flow_codemods::annotate_optional_properties::error_stats::ErrorStats as AnnotateOptionalPropertiesStats;
use flow_codemods::remove_react_import::Acc as RemoveReactImportAcc;
use flow_codemods::remove_react_import::RemoveReactImportMapper;
use flow_codemods::utils::codemod_report;
use flow_codemods::utils::codemod_runner;
use flow_codemods::utils::codemod_runner::SimpleTypedRunnerConfig;
use flow_codemods::utils::codemod_runner::TypedRunnerWithPrepassConfig;
use flow_codemods::utils::codemod_runner::UntypedRunnerConfig;
use flow_codemods::utils::codemod_utils::MakeMain;
use flow_common::flow_projects::FlowProjects;
use flow_common::options::Options;
use flow_common_modulename::HasteModuleInfo;
use flow_common_modulename::Modulename;
use flow_parser::ast;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser_utils::flow_ast_differ;
use flow_parser_utils_output::js_layout_generator::Opts as LayoutOptions;
use flow_parser_utils_output::replacement_printer;
use flow_server_env::monitor_rpc;
use flow_server_utils::file_input::FileInput;
use flow_services_autocomplete::module_system_info::LspModuleSystemInfo;
use flow_services_code_action::code_action_service;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;
use tracing::level_filters::LevelFilter;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

#[derive(Clone)]
enum CodemodSubcommand {
    AnnotateExports,
    AnnotateLiteralDeclaration,
    AnnotateOptionalProperties,
    FixErrors,
    RemoveUnnecessaryReactImport,
}

#[derive(Clone, Debug)]
struct AnnotateRuntimeConfig {
    max_type_size: i32,
    default_any: bool,
}

#[derive(Clone)]
pub(crate) struct PreparedCodemod {
    pub(crate) log_level: Option<LevelFilter>,
    pub(crate) options: Options,
    pub(crate) repeat: bool,
    pub(crate) roots: BTreeSet<FileKey>,
    pub(crate) write: bool,
}

#[derive(Clone)]
struct AnnotateLiteralAccumulator {
    acc: flow_services_code_action::insert_type_utils::Acc<AnnotateLiteralStats>,
}

impl AnnotateLiteralAccumulator {
    fn empty() -> Self {
        Self {
            acc: flow_services_code_action::insert_type_utils::Acc::empty(),
        }
    }

    fn combine(a: Self, b: Self) -> Self {
        Self {
            acc: flow_services_code_action::insert_type_utils::Acc::combine(&a.acc, &b.acc),
        }
    }

    fn report(&self) -> String {
        self.acc.report()
    }
}

static ENVIRONMENT_INITIALIZED: OnceLock<()> = OnceLock::new();

fn initialize_environment() {
    ENVIRONMENT_INITIALIZED.get_or_init(|| {
        // Kickstart daemon processes
        crate::check_entry_point();
        // Disable monitor updates as this is a single-use tool
        monitor_rpc::disable();
        // Improve backtraces
        // Mark the environment as initialized
    });
}

pub(crate) fn parse_i32_flag(value: Option<String>, flag_name: &str, default: i32) -> i32 {
    match value {
        None => default,
        Some(value) => value.parse().unwrap_or_else(|_| {
            eprintln!("Invalid value for {}: {}", flag_name, value);
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            );
        }),
    }
}

fn split_csv_flag(value: Option<String>) -> Vec<String> {
    value
        .map(|value| {
            value
                .split(',')
                .filter(|part| !part.is_empty())
                .map(str::to_string)
                .collect()
        })
        .unwrap_or_default()
}

pub(crate) fn codemod_common_spec(
    name: &str,
    doc: &str,
    visibility: command_spec::Visibility,
    usage: String,
) -> command_spec::Spec {
    command_utils::add_codemod_flags(command_spec::Spec::new(name, doc, visibility, usage))
}

fn prepare_root(flowconfig_name: &str, root_arg: Option<String>, filenames: &[String]) -> PathBuf {
    match root_arg {
        None => command_utils::guess_root(
            flowconfig_name,
            filenames.first().map(|filename| filename.as_str()),
        ),
        Some(provided_root) => {
            let dir = PathBuf::from(command_utils::expand_path(&provided_root));
            if dir.join(flowconfig_name).exists() {
                dir
            } else {
                eprintln!("Invalid root directory {}", provided_root);
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::CouldNotFindFlowconfig,
                );
            }
        }
    }
}

pub(crate) fn prepare_codemod(
    args: &arg_spec::Values,
    default_dot_when_empty: bool,
) -> PreparedCodemod {
    initialize_environment();

    let codemod_flags = command_utils::get_codemod_flags(args);
    let command_utils::CodemodParams {
        options_flags,
        saved_state_options_flags,
        shm_flags: _shm_flags,
        ignore_version,
        write,
        repeat,
        log_level,
        root: root_arg,
        input_file,
        base_flag,
        anon,
    } = codemod_flags;
    let flowconfig_name = base_flag.flowconfig_name;
    // Normalizes filepaths (symlinks and shortcuts)
    let mut filenames =
        command_utils::get_filenames_from_input(false, input_file.as_deref(), anon.as_deref());
    if default_dot_when_empty && filenames.is_empty() {
        filenames.push(".".to_string());
    }
    if filenames.is_empty() {
        eprintln!("Error: filenames or --input-file are required");
        // EX_USAGE
        std::process::exit(64);
    }
    if !write && repeat {
        eprintln!("Error: cannot run codemod with --repeat flag unless --write is also passed");
        flow_common_exit_status::exit(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
        );
    }

    let root = prepare_root(&flowconfig_name, root_arg, &filenames);
    let flowconfig_path = root.join(&flowconfig_name);
    let (flowconfig, flowconfig_hash) = command_utils::read_config_and_hash_or_exit(
        &flowconfig_path.to_string_lossy(),
        !ignore_version,
    );
    let mut options = command_utils::make_options(
        flowconfig,
        flowconfig_hash,
        flowconfig_name,
        root.clone(),
        command_utils::get_temp_dir(&options_flags.temp_dir),
        options_flags.no_flowlib,
        command_utils::MakeOptionsOverrides {
            all: if args.contains_key("--all") {
                Some(options_flags.all)
            } else {
                None
            },
            debug: options_flags.debug,
            distributed: options_flags.distributed,
            estimate_recheck_time: options_flags.estimate_recheck_time,
            flowconfig_flags: Some(options_flags.flowconfig_flags.clone()),
            lazy_mode: Some(flow_config::LazyMode::Lazy),
            include_warnings: options_flags.include_warnings,
            max_warnings: options_flags.max_warnings,
            merge_timeout: options_flags.merge_timeout,
            munge_underscore_members: options_flags.munge_underscore_members,
            no_autoimports: options_flags.no_autoimports,
            profile: Some(options_flags.profile),
            quiet: options_flags.quiet,
            saved_state_fetcher: saved_state_options_flags.saved_state_fetcher,
            saved_state_force_recheck: Some(saved_state_options_flags.saved_state_force_recheck),
            saved_state_no_fallback: Some(saved_state_options_flags.saved_state_no_fallback),
            saved_state_skip_version_check: Some(
                saved_state_options_flags.saved_state_skip_version_check,
            ),
            saved_state_verify: Some(saved_state_options_flags.saved_state_verify),
            slow_to_check_logging: Some(options_flags.slow_to_check_logging.clone()),
            strip_root: options_flags.strip_root,
            verbose: options_flags.verbose.clone(),
            vpn_less: options_flags.vpn_less,
            ..Default::default()
        },
    );
    options.strip_root = options_flags.strip_root;
    options.quiet = options_flags.quiet;
    options.debug = options_flags.debug;
    if let Some(max_workers) = options_flags.max_workers {
        options.max_workers = max_workers;
    }

    let roots = command_utils::expand_file_list(&filenames, Some(&options.file_options))
        .into_iter()
        .map(|filename| {
            flow_common::files::filename_from_string(
                &options.file_options,
                true,
                &BTreeSet::new(),
                &filename,
            )
        })
        .collect();
    PreparedCodemod {
        log_level,
        options,
        repeat,
        roots,
        write,
    }
}

fn string_reporter<A: Clone + Send + Sync + 'static>(
    report: impl Fn(&A) -> String + Send + Sync + 'static,
    combine: impl Fn(A, A) -> A + Send + Sync + 'static,
    empty: A,
) -> codemod_report::CodemodReport<A> {
    codemod_report::CodemodReport {
        report: codemod_report::Reporter::StringReporter(Box::new(move |_, value| report(value))),
        combine: Box::new(combine),
        empty,
    }
}

fn insert_type_acc_reporter<Extra>()
-> codemod_report::CodemodReport<flow_services_code_action::insert_type_utils::Acc<Extra>>
where
    Extra: flow_services_code_action::insert_type_utils::BaseStats + Send + Sync + 'static,
{
    string_reporter(
        |value: &flow_services_code_action::insert_type_utils::Acc<Extra>| value.report(),
        |a, b| flow_services_code_action::insert_type_utils::Acc::combine(&a, &b),
        flow_services_code_action::insert_type_utils::Acc::empty(),
    )
}

fn untyped_acc_reporter<Extra>()
-> codemod_report::CodemodReport<flow_services_code_action::insert_type_utils::UntypedAcc<Extra>>
where
    Extra: flow_services_code_action::insert_type_utils::BaseStats + Send + Sync + 'static,
{
    string_reporter(
        |value: &flow_services_code_action::insert_type_utils::UntypedAcc<Extra>| value.report(),
        |a, b| flow_services_code_action::insert_type_utils::UntypedAcc::combine(&a, &b),
        flow_services_code_action::insert_type_utils::UntypedAcc::empty(),
    )
}

fn save_ast_diff(
    options: &Options,
    file: &FileKey,
    ast: &ast::Program<Loc, Loc>,
    ast_prime: &ast::Program<Loc, Loc>,
) {
    let diff = flow_ast_differ::program(ast, ast_prime);
    if diff.is_empty() {
        return;
    }
    let file_input = FileInput::FileName(file.to_absolute());
    let patch = replacement_printer::mk_patch_ast_differ_unsafe(
        &LayoutOptions {
            preserve_formatting: true,
            ..flow_services_code_action::code_action_utils::layout_options(options)
        },
        &diff,
        &file_input,
    );
    flow_codemods::utils::diff_heaps::set_diff(file, &patch);
}

fn mk_module_system_info(
    options: &Options,
    shared_mem: Arc<flow_heap::parsing_heaps::SharedMem>,
) -> LspModuleSystemInfo {
    let node_resolver_root_relative_dirnames = if options.node_resolver_allow_root_relative {
        let root = options.root.to_string_lossy().to_string();
        options
            .node_resolver_root_relative_dirnames
            .iter()
            .map(|(prefix_opt, root_relative)| {
                (
                    prefix_opt.clone(),
                    flow_common::files::normalize_path(&root, root_relative),
                )
            })
            .collect()
    } else {
        vec![]
    };
    let shared_mem_clone = shared_mem.clone();
    let shared_mem_clone2 = shared_mem.clone();
    let shared_mem_clone3 = shared_mem.clone();
    let projects_options = options.projects_options.dupe();
    LspModuleSystemInfo {
        file_options: options.file_options.dupe(),
        haste_module_system: options.module_system == flow_common::options::ModuleSystem::Haste,
        get_haste_module_info: Arc::new(move |f| shared_mem_clone.get_haste_module_info(f)),
        get_package_info: Box::new(move |f| {
            shared_mem_clone2
                .get_package_info(f)
                .map(|package| Ok((*package).clone()))
        }),
        is_package_file: Box::new(move |module_path, module_name| {
            let dependency = FlowProjects::from_path(
                &projects_options,
                &flow_parser::file_key::strip_project_root(module_path),
            )
            .and_then(|namespace| {
                shared_mem_clone3.get_dependency(&Modulename::Haste(HasteModuleInfo::mk(
                    module_name.into(),
                    namespace.to_bitset(),
                )))
            });
            match dependency.and_then(|dependency| shared_mem_clone3.get_provider(&dependency)) {
                Some(addr) => shared_mem_clone3.is_package_file(&addr),
                None => false,
            }
        }),
        node_resolver_root_relative_dirnames,
        resolves_to_real_path: Box::new(|from, to_real_path| {
            std::fs::canonicalize(from)
                .ok()
                .map(|path| path.to_string_lossy().to_string())
                == Some(to_real_path.to_string())
        }),
    }
}

// ***********************
// Codemod subcommands
// ***********************

mod annotate_exports_command {
    use super::*;

    static RUNTIME_CONFIG: OnceLock<AnnotateRuntimeConfig> = OnceLock::new();

    fn spec() -> command_spec::Spec {
        let doc = "Annotates parts of input that are visible from the exports as required by Flow types-first mode.";
        codemod_common_spec(
            "annotate-exports",
            doc,
            command_spec::Visibility::Public,
            format!(
                "Usage: {} codemod annotate-exports [OPTION]... [FILE]\n\nAnnotates parts of input that are visible from the exports as required by Flow types-first mode.\n",
                command_utils::exe_name()
            ),
        )
        .flag(
            "--max-type-size",
            &arg_spec::optional(arg_spec::string()),
            "The maximum number of nodes allowed in a single type annotation (default: 100)",
            None,
        )
        .flag(
            "--default-any",
            &arg_spec::truthy(),
            "Adds 'any' to all locations where normalization or validation fails",
            None,
        )
    }

    fn visit<'cx>(
        options: &Options,
        ast: &ast::Program<Loc, Loc>,
        cctx: flow_codemods::utils::codemod_context::typed::TypedCodemodContext<'cx>,
    ) -> flow_services_code_action::insert_type_utils::Acc<SignatureVerificationErrorStats> {
        let config = RUNTIME_CONFIG.get().unwrap();
        let mut mapper =
            AnnotateExportsMapper::new(config.max_type_size, config.default_any, &cctx);
        let ast_prime = mapper.program(ast);
        save_ast_diff(options, &cctx.file, ast, &ast_prime);
        mapper.acc()
    }

    struct RunnerConfig;

    impl SimpleTypedRunnerConfig for RunnerConfig {
        type Accumulator =
            flow_services_code_action::insert_type_utils::Acc<SignatureVerificationErrorStats>;

        fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
            insert_type_acc_reporter()
        }

        fn expand_roots(
            _env: &flow_server_env::server_env::Env,
            roots: BTreeSet<FileKey>,
        ) -> BTreeSet<FileKey> {
            roots
        }

        fn check_options(options: Options) -> Options {
            options
        }

        fn visit(
            options: &Options,
            ast: &ast::Program<Loc, Loc>,
            cctx: flow_codemods::utils::codemod_context::typed::TypedCodemodContext<'_>,
        ) -> Self::Accumulator {
            annotate_exports_command::visit(options, ast, cctx)
        }
    }

    fn main(args: &arg_spec::Values) {
        let max_type_size = parse_i32_flag(
            command_spec::get(
                args,
                "--max-type-size",
                &arg_spec::optional(arg_spec::string()),
            )
            .unwrap(),
            "--max-type-size",
            100,
        );
        let default_any = command_spec::get(args, "--default-any", &arg_spec::truthy()).unwrap();
        RUNTIME_CONFIG
            .set(AnnotateRuntimeConfig {
                max_type_size,
                default_any,
            })
            .expect("annotate-exports config already initialized");
        let prepared = prepare_codemod(args, false);
        MakeMain::<codemod_runner::MakeSimpleTypedRunner<RunnerConfig>>::main(
            &prepared.options,
            prepared.write,
            prepared.repeat,
            prepared.log_level,
            prepared.roots,
        );
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

mod annotate_literal_declaration_command {
    use super::*;

    #[derive(Clone, Debug)]
    struct RuntimeConfig {
        max_type_size: i32,
    }

    static RUNTIME_CONFIG: OnceLock<RuntimeConfig> = OnceLock::new();

    fn spec() -> command_spec::Spec {
        let doc = "Annotates literal declaration to fix natural inference errors.";
        codemod_common_spec(
            "annotate-literal-declaration",
            doc,
            command_spec::Visibility::Public,
            format!(
                "Usage: {} codemod annotate-literal-declaration [OPTION]... [FILE]\n\nAnnotates literal declaration to fix natural inference errors.\n",
                command_utils::exe_name()
            ),
        )
        // Use 1 to ensure that we only add type aliased object type without intersections.
        .flag(
            "--max-type-size",
            &arg_spec::optional(arg_spec::string()),
            "The maximum number of nodes allowed in a single type annotation (default: 1, should be increased to at least 2 to insert array annotations.)",
            None,
        )
    }

    fn visit<'cx>(
        options: &Options,
        ast: &ast::Program<Loc, Loc>,
        cctx: flow_codemods::utils::codemod_context::typed::TypedCodemodContext<'cx>,
    ) -> AnnotateLiteralAccumulator {
        let config = RUNTIME_CONFIG.get().unwrap();
        let mut mapper = AnnotateLiteralDeclarationsMapper::new(config.max_type_size, &cctx);
        let ast_prime = mapper.program(ast.clone());
        save_ast_diff(options, &cctx.file, ast, &ast_prime);
        let mut acc = flow_services_code_action::insert_type_utils::Acc::empty();
        acc.stats.extra = mapper.post_run();
        AnnotateLiteralAccumulator { acc }
    }

    struct RunnerConfig;

    impl SimpleTypedRunnerConfig for RunnerConfig {
        type Accumulator = AnnotateLiteralAccumulator;

        fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
            string_reporter(
                |value: &AnnotateLiteralAccumulator| value.report(),
                AnnotateLiteralAccumulator::combine,
                AnnotateLiteralAccumulator::empty(),
            )
        }

        fn expand_roots(
            _env: &flow_server_env::server_env::Env,
            roots: BTreeSet<FileKey>,
        ) -> BTreeSet<FileKey> {
            roots
        }

        fn check_options(options: Options) -> Options {
            options
        }

        fn visit(
            options: &Options,
            ast: &ast::Program<Loc, Loc>,
            cctx: flow_codemods::utils::codemod_context::typed::TypedCodemodContext<'_>,
        ) -> Self::Accumulator {
            annotate_literal_declaration_command::visit(options, ast, cctx)
        }
    }

    fn main(args: &arg_spec::Values) {
        let max_type_size = parse_i32_flag(
            command_spec::get(
                args,
                "--max-type-size",
                &arg_spec::optional(arg_spec::string()),
            )
            .unwrap(),
            "--max-type-size",
            1,
        );
        RUNTIME_CONFIG
            .set(RuntimeConfig { max_type_size })
            .expect("annotate-literal-declaration config already initialized");
        let prepared = prepare_codemod(args, false);
        MakeMain::<codemod_runner::MakeSimpleTypedRunner<RunnerConfig>>::main(
            &prepared.options,
            prepared.write,
            prepared.repeat,
            prepared.log_level,
            prepared.roots,
        );
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

mod remove_react_import_command {
    use super::*;

    fn spec() -> command_spec::Spec {
        let doc = "Remove unnecessary imports of React under react.runtime=automatic.";
        codemod_common_spec(
            "remove-unnecessary-react-import",
            doc,
            command_spec::Visibility::Public,
            format!(
                "Usage: {} codemod remove-unnecessary-react-import [OPTION]... [FILE]\n\nRemove unnecessary imports of React under react.runtime=automatic.\n",
                command_utils::exe_name()
            ),
        )
    }

    struct RunnerConfig;

    impl UntypedRunnerConfig for RunnerConfig {
        type Accumulator = RemoveReactImportAcc;

        fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
            untyped_acc_reporter()
        }

        fn visit(
            options: &Options,
            ast: &ast::Program<Loc, Loc>,
            cctx: flow_codemods::utils::codemod_context::untyped::UntypedCodemodContext,
        ) -> Self::Accumulator {
            let mut mapper = RemoveReactImportMapper::new(cctx.file.clone());
            let ast_prime = mapper.program(ast.clone());
            save_ast_diff(options, &cctx.file, ast, &ast_prime);
            mapper.inner.acc
        }
    }

    fn main(args: &arg_spec::Values) {
        let prepared = prepare_codemod(args, false);
        MakeMain::<codemod_runner::MakeUntypedRunner<RunnerConfig>>::main(
            &prepared.options,
            prepared.write,
            prepared.repeat,
            prepared.log_level,
            prepared.roots,
        );
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

mod annotate_optional_properties_command {
    use super::*;

    static RUNTIME_CONFIG: OnceLock<AnnotateRuntimeConfig> = OnceLock::new();

    fn spec() -> command_spec::Spec {
        let doc = "Inserts optional properties on object definitions where properties are missing.";
        codemod_common_spec(
            "annotate-optional-properties",
            doc,
            command_spec::Visibility::Public,
            format!(
                "Usage: {} codemod annotate-optional-properties [OPTION]... [FILE]\n\nInserts optional properties on object definitions where properties are missing.\n",
                command_utils::exe_name()
            ),
        )
        .flag(
            "--max-type-size",
            &arg_spec::optional(arg_spec::string()),
            "The maximum number of nodes allowed in a single type annotation (default: 100)",
            None,
        )
        .flag(
            "--default-any",
            &arg_spec::truthy(),
            "Adds 'any' to all locations where normalization or validation fails",
            None,
        )
    }

    fn visit<'cx>(
        options: &Options,
        ast: &ast::Program<Loc, Loc>,
        cctx: flow_codemods::utils::codemod_context::typed::TypedCodemodContext<'cx>,
    ) -> flow_services_code_action::insert_type_utils::Acc<AnnotateOptionalPropertiesStats> {
        let config = RUNTIME_CONFIG.get().unwrap();
        let mut mapper =
            AnnotateOptionalPropertiesMapper::new(config.max_type_size, config.default_any, &cctx);
        let ast_prime = mapper.program(ast.clone());
        save_ast_diff(options, &cctx.file, ast, &ast_prime);
        mapper.acc()
    }

    struct RunnerConfig;

    impl SimpleTypedRunnerConfig for RunnerConfig {
        type Accumulator =
            flow_services_code_action::insert_type_utils::Acc<AnnotateOptionalPropertiesStats>;

        fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
            insert_type_acc_reporter()
        }

        fn expand_roots(
            _env: &flow_server_env::server_env::Env,
            roots: BTreeSet<FileKey>,
        ) -> BTreeSet<FileKey> {
            roots
        }

        fn check_options(options: Options) -> Options {
            options
        }

        fn visit(
            options: &Options,
            ast: &ast::Program<Loc, Loc>,
            cctx: flow_codemods::utils::codemod_context::typed::TypedCodemodContext<'_>,
        ) -> Self::Accumulator {
            annotate_optional_properties_command::visit(options, ast, cctx)
        }
    }

    fn main(args: &arg_spec::Values) {
        let max_type_size = parse_i32_flag(
            command_spec::get(
                args,
                "--max-type-size",
                &arg_spec::optional(arg_spec::string()),
            )
            .unwrap(),
            "--max-type-size",
            100,
        );
        let default_any = command_spec::get(args, "--default-any", &arg_spec::truthy()).unwrap();
        RUNTIME_CONFIG
            .set(AnnotateRuntimeConfig {
                max_type_size,
                default_any,
            })
            .expect("annotate-optional-properties config already initialized");
        let prepared = prepare_codemod(args, false);
        MakeMain::<codemod_runner::MakeSimpleTypedRunner<RunnerConfig>>::main(
            &prepared.options,
            prepared.write,
            prepared.repeat,
            prepared.log_level,
            prepared.roots,
        );
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

mod fix_errors_command {
    use super::*;

    #[derive(Clone, Debug)]
    struct RuntimeConfig {
        error_codes: Option<BTreeSet<String>>,
    }

    static RUNTIME_CONFIG: OnceLock<RuntimeConfig> = OnceLock::new();

    // transformable errors are a subset of all errors; specifically,
    // the errors for which Code_action_service.ast_transforms_of_error is non-empty
    type TransformableError = flow_typing_errors::error_message::ErrorMessage<Loc>;
    type TransformableErrorsMap = BTreeMap<FileKey, Vec<TransformableError>>;
    // Codemod-specific shared mem heap
    // stores a mapping from a file to all the errors that have transformations in that file
    static FIX_ERRORS_HEAP: OnceLock<Mutex<TransformableErrorsMap>> = OnceLock::new();

    fn union_transformable_errors_maps(
        mut left: TransformableErrorsMap,
        right: TransformableErrorsMap,
    ) -> TransformableErrorsMap {
        for (file, mut errors) in right {
            left.entry(file).or_default().append(&mut errors);
        }
        left
    }

    struct RunnerConfig;

    impl TypedRunnerWithPrepassConfig for RunnerConfig {
        type Accumulator = ();
        type PrepassState = ();
        type PrepassResult = TransformableErrorsMap;

        fn reporter() -> codemod_report::CodemodReport<Self::Accumulator> {
            codemod_report::unit_reporter()
        }

        fn expand_roots(
            _env: &flow_server_env::server_env::Env,
            roots: BTreeSet<FileKey>,
        ) -> BTreeSet<FileKey> {
            roots
        }

        fn check_options(options: Options) -> Options {
            options
        }

        fn prepass_init() -> Self::PrepassState {}

        fn mod_prepass_options(options: Options) -> Options {
            options
        }

        fn include_dependents_in_prepass() -> bool {
            false
        }

        fn prepass_run(
            cx: Context<'_>,
            _state: &Self::PrepassState,
            _file: FileKey,
            _file_options: &Arc<flow_common::files::FileOptions>,
            reader: &Arc<flow_heap::parsing_heaps::SharedMem>,
            _file_sig: &Arc<flow_parser_utils::file_sig::FileSig>,
            _typed_ast: &ast::Program<flow_aloc::ALoc, (flow_aloc::ALoc, Type)>,
        ) -> Self::PrepassResult {
            let config = RUNTIME_CONFIG.get().unwrap();
            let loc_of_aloc = {
                let reader = reader.clone();
                Arc::new(move |aloc: &flow_aloc::ALoc| reader.loc_of_aloc(aloc))
            };
            let get_ast_from_shared_mem = {
                let reader = reader.clone();
                Arc::new(move |file: &FileKey| reader.get_ast(file).map(|ast| (*ast).clone()))
            };
            let get_type_sig = {
                let reader = reader.clone();
                Arc::new(move |file: &FileKey| {
                    reader.get_type_sig(file).map(|type_sig| {
                        let bytes =
                            bincode::serde::encode_to_vec(&*type_sig, bincode::config::legacy())
                                .expect("get_type_sig: serialize");
                        bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                            .expect("get_type_sig: deserialize")
                            .0
                    })
                })
            };
            let options = Options {
                file_options: cx.metadata().frozen.file_options.dupe(),
                root: cx.metadata().frozen.root.dupe(),
                ..Options::default()
            };
            let module_system_info = mk_module_system_info(&options, reader.clone());
            let include_error: Box<
                dyn Fn(&flow_typing_errors::error_message::ErrorMessage<Loc>) -> bool,
            > = match &config.error_codes {
                None => Box::new(|_| true),
                Some(codes) if codes.is_empty() => Box::new(|_| true),
                Some(codes) => {
                    let codes = codes.clone();
                    Box::new(
                        move |error_message| match error_message.error_code_of_message() {
                            None => false,
                            Some(error_code) => codes.contains(error_code.as_str()),
                        },
                    )
                }
            };
            cx.errors().fold(
                BTreeMap::new(),
                |acc: TransformableErrorsMap,
                 error: flow_typing_errors::flow_error::FlowError<flow_aloc::ALoc>| {
                    let lazy_error_loc =
                        flow_typing_errors::intermediate_error::make_intermediate_error(
                            |aloc: &flow_aloc::ALoc| loc_of_aloc(aloc),
                            false,
                            &error,
                        )
                        .loc;
                    let error_message =
                        flow_typing_errors::error_message::ErrorMessage::map_loc_of_error_message(
                            |aloc: flow_aloc::ALoc| loc_of_aloc(&aloc),
                            error.msg_of_error().clone(),
                        );
                    let transforms = code_action_service::ast_transforms_of_error(
                        loc_of_aloc.dupe(),
                        Some(lazy_error_loc),
                        get_ast_from_shared_mem.dupe(),
                        module_system_info.get_haste_module_info.dupe(),
                        get_type_sig.dupe(),
                        None,
                        &error_message,
                    );
                    match transforms.as_slice() {
                        // TODO(T138883537): There should be a way to configure which fix to apply
                        [
                            flow_services_code_action::code_action_service::AstTransformOfError {
                                target_loc,
                                ..
                            },
                        ] if include_error(&error_message) => {
                            match target_loc.source().cloned() {
                                Some(file_key) => {
                                    let mut map = acc;
                                    map.entry(file_key).or_default().push(error_message);
                                    map
                                }
                                None => acc,
                            }
                        }
                        _ => acc,
                    }
                },
            )
        }

        fn store_precheck_result(
            results: BTreeMap<
                FileKey,
                flow_codemods::utils::codemod_runner::UnitResult<Self::PrepassResult>,
            >,
        ) {
            let merged = results
                .into_values()
                .filter_map(Result::ok)
                .fold(BTreeMap::new(), union_transformable_errors_maps);
            let heap = FIX_ERRORS_HEAP.get_or_init(|| Mutex::new(BTreeMap::new()));
            *heap.lock().unwrap() = merged;
        }

        fn visit(
            options: &Options,
            ast: &ast::Program<Loc, Loc>,
            cctx: flow_codemods::utils::codemod_context::typed::TypedCodemodContext<'_>,
        ) -> Self::Accumulator {
            let transformable_errors = FIX_ERRORS_HEAP
                .get_or_init(|| Mutex::new(BTreeMap::new()))
                .lock()
                .unwrap()
                .get(&cctx.file)
                .cloned();
            let Some(transformable_errors) = transformable_errors else {
                return;
            };
            let loc_of_aloc = {
                let reader = cctx.reader.clone();
                Arc::new(move |aloc: &flow_aloc::ALoc| reader.loc_of_aloc(aloc))
            };
            let get_ast_from_shared_mem = {
                let reader = cctx.reader.clone();
                Arc::new(move |file: &FileKey| reader.get_ast(file).map(|ast| (*ast).clone()))
            };
            let get_type_sig = {
                let reader = cctx.reader.clone();
                Arc::new(move |file: &FileKey| {
                    reader.get_type_sig(file).map(|type_sig| {
                        let bytes =
                            bincode::serde::encode_to_vec(&*type_sig, bincode::config::legacy())
                                .expect("get_type_sig: serialize");
                        bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                            .expect("get_type_sig: deserialize")
                            .0
                    })
                })
            };
            let module_system_info = mk_module_system_info(options, cctx.reader.clone());
            let ast_prime =
                transformable_errors
                    .into_iter()
                    .fold(ast.clone(), |current_ast, error_message| {
                        // TODO(T138883537): There should be a way to configure which fix to apply
                        let transforms = code_action_service::ast_transforms_of_error(
                            loc_of_aloc.dupe(),
                            None,
                            get_ast_from_shared_mem.dupe(),
                            module_system_info.get_haste_module_info.dupe(),
                            get_type_sig.dupe(),
                            None,
                            &error_message,
                        );
                        let transform = transforms
                            .into_iter()
                            .next()
                            .expect("ast_transforms_of_error returned empty list");
                        let result = (transform.transform)(
                            &cctx.cx,
                            cctx.file_sig.dupe(),
                            &current_ast,
                            &cctx.typed_ast,
                            transform.target_loc,
                        );
                        result.unwrap_or(current_ast)
                    });
            save_ast_diff(options, &cctx.file, ast, &ast_prime);
        }
    }

    fn spec() -> command_spec::Spec {
        let doc = "Automatically fixes transformable Flow errors.";
        codemod_common_spec(
            "fix-errors",
            doc,
            command_spec::Visibility::Public,
            format!(
                "Usage: {} codemod fix-errors [OPTION]... [FILE]\n\nAutomatically fixes transformable Flow errors.\n",
                command_utils::exe_name()
            ),
        )
        .flag(
            "--error-codes",
            &arg_spec::optional(arg_spec::string()),
            "Only transform errors with the provided comma-separated error codes",
            None,
        )
    }

    fn main(args: &arg_spec::Values) {
        let error_codes = command_spec::get(
            args,
            "--error-codes",
            &arg_spec::optional(arg_spec::string()),
        )
        .unwrap();
        RUNTIME_CONFIG
            .set(RuntimeConfig {
                error_codes: {
                    let codes = split_csv_flag(error_codes);
                    if codes.is_empty() {
                        None
                    } else {
                        Some(codes.into_iter().collect())
                    }
                },
            })
            .expect("fix-errors config already initialized");
        let prepared = prepare_codemod(args, true);
        MakeMain::<codemod_runner::MakeTypedRunnerWithPrepass<RunnerConfig>>::main(
            &prepared.options,
            prepared.write,
            prepared.repeat,
            prepared.log_level,
            prepared.roots,
        );
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

fn root_spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "codemod",
        "Runs large-scale codebase refactors",
        command_spec::Visibility::Public,
        format!(
            "Usage: {} codemod SUBCOMMAND [OPTION]...",
            command_utils::exe_name()
        ),
    )
    .anon(
        "subcommand",
        &arg_spec::command_flag(vec![
            ("annotate-exports", CodemodSubcommand::AnnotateExports),
            (
                "annotate-literal-declaration",
                CodemodSubcommand::AnnotateLiteralDeclaration,
            ),
            (
                "annotate-optional-properties",
                CodemodSubcommand::AnnotateOptionalProperties,
            ),
            ("fix-errors", CodemodSubcommand::FixErrors),
            (
                "remove-unnecessary-react-import",
                CodemodSubcommand::RemoveUnnecessaryReactImport,
            ),
        ]),
    )
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(root_spec(), |args| {
        let (subcommand, argv) = command_spec::get(
            args,
            "subcommand",
            &arg_spec::command_flag(vec![
                ("annotate-exports", CodemodSubcommand::AnnotateExports),
                (
                    "annotate-literal-declaration",
                    CodemodSubcommand::AnnotateLiteralDeclaration,
                ),
                (
                    "annotate-optional-properties",
                    CodemodSubcommand::AnnotateOptionalProperties,
                ),
                ("fix-errors", CodemodSubcommand::FixErrors),
                (
                    "remove-unnecessary-react-import",
                    CodemodSubcommand::RemoveUnnecessaryReactImport,
                ),
            ]),
        )
        .unwrap()
        .unwrap();
        let command = match subcommand {
            CodemodSubcommand::AnnotateExports => annotate_exports_command::command(),
            CodemodSubcommand::AnnotateLiteralDeclaration => {
                annotate_literal_declaration_command::command()
            }
            CodemodSubcommand::AnnotateOptionalProperties => {
                annotate_optional_properties_command::command()
            }
            CodemodSubcommand::FixErrors => fix_errors_command::command(),
            CodemodSubcommand::RemoveUnnecessaryReactImport => {
                remove_react_import_command::command()
            }
        };
        command_utils::run_command(&command, &argv);
    })
}
