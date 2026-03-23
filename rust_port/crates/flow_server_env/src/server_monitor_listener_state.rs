/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Ported from src/server/monitor_listener/serverMonitorListenerState.ml

use std::collections::BTreeSet;
use std::sync::Mutex;

use crossbeam::channel::Receiver;
use crossbeam::channel::Sender;
use dupe::Dupe;
use flow_common_utils::checked_set::CheckedSet;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;

use crate::server_env::Env;
use crate::workload_stream::ParallelizableWorkload;
use crate::workload_stream::Workload;
use crate::workload_stream::WorkloadHandler;
use crate::workload_stream::WorkloadStream;

// module FilenameSet = Utils_js.FilenameSet

// type env_update = ServerEnv.env -> ServerEnv.env
pub type EnvUpdate = Box<dyn FnOnce(Env) -> Env + Send>;

// Workloads are client requests which we processes FIFO
static WORKLOAD_STREAM: std::sync::LazyLock<WorkloadStream> =
    std::sync::LazyLock::new(WorkloadStream::create);

static WORKLOAD_NOTIFY: std::sync::LazyLock<(Sender<()>, Receiver<()>)> =
    std::sync::LazyLock::new(crossbeam::channel::unbounded);
static ENV_UPDATE_NOTIFY: std::sync::LazyLock<(Sender<()>, Receiver<()>)> =
    std::sync::LazyLock::new(crossbeam::channel::unbounded);
static RECHECK_NOTIFY: std::sync::LazyLock<(Sender<()>, Receiver<()>)> =
    std::sync::LazyLock::new(crossbeam::channel::unbounded);

pub fn push_new_workload(name: &str, workload: Workload) {
    WORKLOAD_STREAM.push(name, workload);
    WORKLOAD_NOTIFY.0.send(()).unwrap();
}

pub fn push_new_parallelizable_workload(name: &str, workload: ParallelizableWorkload) {
    WORKLOAD_STREAM.push_parallelizable(name, workload);
    WORKLOAD_NOTIFY.0.send(()).unwrap();
}

static DEFERRED_PARALLELIZABLE_WORKLOADS: Mutex<Vec<(String, ParallelizableWorkload)>> =
    Mutex::new(Vec::new());

pub fn defer_parallelizable_workload(name: &str, workload: ParallelizableWorkload) {
    let mut guard = DEFERRED_PARALLELIZABLE_WORKLOADS.lock().unwrap();
    guard.push((name.to_string(), workload));
}

pub fn requeue_deferred_parallelizable_workloads() {
    let workloads = {
        let mut guard = DEFERRED_PARALLELIZABLE_WORKLOADS.lock().unwrap();
        std::mem::take(&mut *guard)
    };
    for (name, workload) in workloads.into_iter().rev() {
        WORKLOAD_STREAM.requeue_parallelizable(&name, workload);
    }
    WORKLOAD_NOTIFY.0.send(()).unwrap();
}

// (* Env updates are...well...updates to our env. They must be handled in the main thread. Also FIFO
//  * but are quick to handle *)
// let (env_update_stream, push_new_env_update) = Lwt_stream.create ()
static ENV_UPDATE_STREAM: Mutex<Vec<EnvUpdate>> = Mutex::new(Vec::new());

// let push_new_env_update env_update = push_new_env_update (Some env_update)
pub fn push_new_env_update(env_update: EnvUpdate) {
    ENV_UPDATE_STREAM.lock().unwrap().push(env_update);
    ENV_UPDATE_NOTIFY.0.send(()).unwrap();
}

// Outstanding cancellation requests are lodged here as soon as they arrive
// from the monitor (NOT FIFO) as well as being lodged in the normal FIFO
// queue. (1) if there was a workload sent prior to the cancellation request
// but we're only just FIFO getting to it now, it can peek to see whether
// that a cancellation request came after it, and not bother starting.
// (2) by the time the FIFO queue gets around to seeing the cancellation
// request in the normal FIFO queue, then we know it can no longer cancel
// any future requests, so we'll remove it from the set.
// Observe that our cancellation handling is best-effort only... we won't
// cancel something already underway, and we might start something even while
// the cancellation request is queued up somewhere between monitor and server.
static CANCELLATION_REQUESTS: Mutex<BTreeSet<String>> = Mutex::new(BTreeSet::new());

pub fn cancellation_requests() -> &'static Mutex<BTreeSet<String>> {
    &CANCELLATION_REQUESTS
}

struct RecheckMsg {
    // file_watcher_metadata: MonitorProt.file_watcher_metadata option;
    // Note: MonitorProt.file_watcher_metadata not yet ported; using unit placeholder.
    // When ported, this will carry file watcher metadata for logging/telemetry.
    _file_watcher_metadata: (),
    files: RecheckFiles,
}

enum RecheckFiles {
    ChangedFiles(BTreeSet<String>, bool),
    FilesToForceFocusedAndRecheck {
        files: BTreeSet<String>,
        /// Normally, a recheck will abort if certain files change in an
        /// incompatible way that can't be handled incrementally. But when
        /// starting a lazy server, we just read the flowconfig, libs, etc.
        /// as part of the init so we shouldn't fail if they are included
        /// in the files changed since mergebase.
        skip_incompatible: bool,
    },
    // Note: Persistent_connection.single_client and LspProt response types not yet ported.
    // Using Loc list for def_locs; the callback and client are represented as placeholder types.
    GlobalFindRef {
        def_locs: Vec<Loc>,
        // request, client, references_to_lsp_response stored as opaque callback
        _find_ref_callback: Box<dyn FnOnce() + Send>,
    },
    DependenciesToPrioritize(FlowOrdSet<FileKey>),
    FilesToReinit {
        files_to_prioritize: FlowOrdSet<FileKey>,
        files_to_recheck: FlowOrdSet<FileKey>,
        files_to_force: CheckedSet,
    },
}

// Files which have changed
static RECHECK_STREAM: Mutex<Vec<RecheckMsg>> = Mutex::new(Vec::new());

fn push_recheck_msg(files: RecheckFiles) {
    RECHECK_STREAM.lock().unwrap().push(RecheckMsg {
        _file_watcher_metadata: (),
        files,
    });
    RECHECK_NOTIFY.0.send(()).unwrap();
}

pub fn push_files_to_recheck(changed_files: BTreeSet<String>) {
    push_recheck_msg(RecheckFiles::ChangedFiles(changed_files, false));
}

pub fn push_files_to_prioritize(changed_files: BTreeSet<String>) {
    push_recheck_msg(RecheckFiles::ChangedFiles(changed_files, true));
}

pub fn push_files_to_force_focused_and_recheck(files: BTreeSet<String>) {
    push_recheck_msg(RecheckFiles::FilesToForceFocusedAndRecheck {
        files,
        skip_incompatible: false,
    });
}

// Note: request/client/references_to_lsp_response bundled as opaque callback until
// Persistent_connection and LspProt are ported.
pub fn push_global_find_ref_request(
    def_locs: Vec<Loc>,
    find_ref_callback: Box<dyn FnOnce() + Send>,
) {
    push_recheck_msg(RecheckFiles::GlobalFindRef {
        def_locs,
        _find_ref_callback: find_ref_callback,
    });
}

/// [push_lazy_init files] triggers a recheck of [files]. It should be called
/// immediately after a lazy init, and [files] should be the files changed
/// since mergebase.  
pub fn push_lazy_init(files: BTreeSet<String>) {
    push_recheck_msg(RecheckFiles::FilesToForceFocusedAndRecheck {
        files,
        skip_incompatible: true,
    });
}

// let push_dependencies_to_prioritize dependencies =
//   push_recheck_msg (DependenciesToPrioritize dependencies)
pub fn push_dependencies_to_prioritize(dependencies: FlowOrdSet<FileKey>) {
    push_recheck_msg(RecheckFiles::DependenciesToPrioritize(dependencies));
}

pub fn push_after_reinit(
    files_to_prioritize: Option<FlowOrdSet<FileKey>>,
    files_to_recheck: Option<FlowOrdSet<FileKey>>,
    files_to_force: Option<CheckedSet>,
) {
    let files_to_prioritize = files_to_prioritize.unwrap_or_default();
    let files_to_recheck = files_to_recheck.unwrap_or_default();
    let files_to_force = files_to_force.unwrap_or_else(CheckedSet::empty);
    push_recheck_msg(RecheckFiles::FilesToReinit {
        files_to_prioritize,
        files_to_recheck,
        files_to_force,
    });
}

pub fn pop_next_workload() -> Option<WorkloadHandler> {
    WORKLOAD_STREAM.pop()
}

pub fn pop_next_parallelizable_workload() -> Option<ParallelizableWorkload> {
    WORKLOAD_STREAM.pop_parallelizable()
}

pub fn update_env(mut env: Env) -> Env {
    let updates: Vec<EnvUpdate> = {
        let mut stream = ENV_UPDATE_STREAM.lock().unwrap();
        std::mem::take(&mut *stream)
    };
    for f in updates {
        env = f(env);
    }
    env
}

pub struct RecheckWorkload {
    pub files_to_prioritize: FlowOrdSet<FileKey>,
    pub files_to_recheck: FlowOrdSet<FileKey>,
    pub files_to_force: CheckedSet,
    // find_ref_command: not yet ported (needs Persistent_connection.single_client, LspProt)
    // metadata: not yet ported (needs MonitorProt.file_watcher_metadata)
    pub require_full_check_reinit: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Priority {
    Priority,
    Normal,
}

pub enum Updates {
    NormalUpdates(FlowOrdSet<FileKey>),
    RequiredFullCheckReinit(FlowOrdSet<FileKey>),
}

fn empty_recheck_workload() -> RecheckWorkload {
    RecheckWorkload {
        files_to_prioritize: FlowOrdSet::new(),
        files_to_recheck: FlowOrdSet::new(),
        files_to_force: CheckedSet::empty(),
        require_full_check_reinit: false,
    }
}

static RECHECK_ACC: Mutex<Option<RecheckWorkload>> = Mutex::new(None);

fn with_recheck_acc<T>(f: impl FnOnce(&mut RecheckWorkload) -> T) -> T {
    let mut guard = RECHECK_ACC.lock().unwrap();
    if guard.is_none() {
        *guard = Some(empty_recheck_workload());
    }
    f(guard.as_mut().unwrap())
}

/// Updates [workload] while maintaining physical equality if there's nothing to do
fn update_workload(
    workload: &mut RecheckWorkload,
    files_to_prioritize: Option<&FlowOrdSet<FileKey>>,
    files_to_recheck: Option<&FlowOrdSet<FileKey>>,
    files_to_force: Option<&CheckedSet>,
) -> bool {
    let mut changed = false;
    if let Some(new_files) = files_to_prioritize {
        let before = workload.files_to_prioritize.len();
        for f in new_files {
            workload.files_to_prioritize.insert(f.dupe());
        }
        if workload.files_to_prioritize.len() != before {
            changed = true;
        }
    }
    if let Some(new_files) = files_to_recheck {
        let before = workload.files_to_recheck.len();
        for f in new_files {
            workload.files_to_recheck.insert(f.dupe());
        }
        if workload.files_to_recheck.len() != before {
            changed = true;
        }
    }
    if let Some(new_force) = files_to_force {
        let before = workload.files_to_force.cardinal();
        workload.files_to_force.union(new_force.dupe());
        if workload.files_to_force.cardinal() != before {
            changed = true;
        }
    }
    changed
}

fn update_to_require_reinit(workload: &mut RecheckWorkload) -> bool {
    if workload.require_full_check_reinit {
        false
    } else {
        workload.require_full_check_reinit = true;
        true
    }
}

pub struct WorkloadChanges {
    pub num_files_to_prioritize: usize,
    pub num_files_to_recheck: usize,
    pub num_files_to_force: usize,
}

fn _summarize_changes(a: &RecheckWorkload, b: &RecheckWorkload) -> WorkloadChanges {
    let num_files_to_prioritize = b.files_to_prioritize.len() - a.files_to_prioritize.len();
    let num_files_to_recheck = b.files_to_recheck.len() - a.files_to_recheck.len();
    let num_files_to_force = b.files_to_force.cardinal() - a.files_to_force.cardinal();
    WorkloadChanges {
        num_files_to_prioritize,
        num_files_to_recheck,
        num_files_to_force,
    }
}

/// Process the messages which are currently in the recheck stream and return the resulting workload
///
/// The recheck stream gives us files as a set of strings. `process_updates` takes that set of
/// strings and returns a `FilenameSet.t`. It filters out stuff we don't care about and causes us to
/// exit on incompatible changes.
///
/// `get_forced` is a function which gives us the `CheckedSet.t` of currently forced files. So if
/// the recheck stream is asking us to focus `foo.js` but it's already focused, then we can ignore
/// it.
pub fn recheck_fetch(
    process_updates: &dyn Fn(bool, &BTreeSet<String>) -> Updates,
    get_forced: &dyn Fn() -> CheckedSet,
    priority: Priority,
) -> bool {
    let msgs: Vec<RecheckMsg> = {
        let mut stream = RECHECK_STREAM.lock().unwrap();
        std::mem::take(&mut *stream)
    };
    let mut changed = false;
    with_recheck_acc(|workload| {
        for msg in msgs {
            let msg_changed = match msg.files {
                RecheckFiles::ChangedFiles(changed_files, urgent) => {
                    match process_updates(false, &changed_files) {
                        Updates::NormalUpdates(updates) => {
                            if urgent {
                                update_workload(workload, Some(&updates), None, None)
                            } else {
                                update_workload(workload, None, Some(&updates), None)
                            }
                        }
                        Updates::RequiredFullCheckReinit(updates) => {
                            let w_changed = if urgent {
                                update_workload(workload, Some(&updates), None, None)
                            } else {
                                update_workload(workload, None, Some(&updates), None)
                            };
                            let r_changed = update_to_require_reinit(workload);
                            w_changed || r_changed
                        }
                    }
                }
                RecheckFiles::FilesToForceFocusedAndRecheck {
                    files,
                    skip_incompatible,
                } => match process_updates(skip_incompatible, &files) {
                    Updates::NormalUpdates(updates) => {
                        let focused = {
                            let forced = get_forced();
                            let forced_focused = forced.focused().dupe();
                            FlowOrdSet::from(
                                updates
                                    .dupe()
                                    .into_inner()
                                    .difference(forced_focused.into_inner()),
                            )
                        };
                        let mut files_to_force = CheckedSet::empty();
                        files_to_force.add(Some(focused), None, None);
                        update_workload(workload, None, Some(&updates), Some(&files_to_force))
                    }
                    Updates::RequiredFullCheckReinit(updates) => {
                        let focused = {
                            let forced = get_forced();
                            let forced_focused = forced.focused().dupe();
                            FlowOrdSet::from(
                                updates
                                    .dupe()
                                    .into_inner()
                                    .difference(forced_focused.into_inner()),
                            )
                        };
                        let mut files_to_force = CheckedSet::empty();
                        files_to_force.add(Some(focused), None, None);
                        let w_changed =
                            update_workload(workload, None, Some(&updates), Some(&files_to_force));
                        let r_changed = update_to_require_reinit(workload);
                        w_changed || r_changed
                    }
                },
                RecheckFiles::GlobalFindRef {
                    def_locs,
                    _find_ref_callback: _,
                } => {
                    let files_to_recheck: FlowOrdSet<FileKey> = def_locs
                        .iter()
                        .filter_map(|loc| loc.source())
                        .cloned()
                        .collect();
                    // Note: find_ref_command update not yet ported
                    update_workload(workload, None, Some(&files_to_recheck), None)
                }
                RecheckFiles::DependenciesToPrioritize(dependencies) => {
                    let mut to_prioritize = CheckedSet::empty();
                    to_prioritize.add(None, None, Some(dependencies));
                    // if we're doing a normal recheck, don't filter out dependencies that are
                    // already being checked, because we want to cancel this recheck and do a
                    // faster priority check. but if we're already doing a priority check, we
                    // don't want to cancel it just to start another with the same files.
                    let files_to_force = match priority {
                        Priority::Normal => to_prioritize,
                        Priority::Priority => {
                            let forced = get_forced();
                            let mut result = to_prioritize;
                            result.diff(&forced);
                            result
                        }
                    };
                    update_workload(workload, None, None, Some(&files_to_force))
                }
                RecheckFiles::FilesToReinit {
                    files_to_prioritize,
                    files_to_recheck,
                    files_to_force,
                } => {
                    // pushing files to reinit should not trigger a "change", because
                    // these files are not caused by a distinct event, like a file
                    // watcher event; they're a continuation of the existing reinit
                    // event.
                    update_workload(
                        workload,
                        Some(&files_to_prioritize),
                        Some(&files_to_recheck),
                        Some(&files_to_force),
                    );
                    false
                }
            };
            changed = changed || msg_changed;
        }
    });
    changed
}

pub fn requeue_workload(workload: RecheckWorkload) {
    eprintln!(
        "Re-queueing force-check of {} files and recheck of {} files ({} dependencies)",
        workload.files_to_force.cardinal(),
        workload
            .files_to_recheck
            .dupe()
            .union(workload.files_to_prioritize.dupe())
            .len(),
        workload.files_to_prioritize.len(),
    );
    with_recheck_acc(|prev| {
        for f in &workload.files_to_prioritize {
            prev.files_to_prioritize.insert(f.dupe());
        }
        for f in &workload.files_to_recheck {
            prev.files_to_recheck.insert(f.dupe());
        }
        prev.files_to_force.union(workload.files_to_force);
        // Note: find_ref_command merge not yet ported
        // Note: metadata merge not yet ported
        if prev.require_full_check_reinit || workload.require_full_check_reinit {
            eprintln!(
                "Previous recheck requires restart: {}; new workload requires restart: {}",
                prev.require_full_check_reinit, workload.require_full_check_reinit,
            );
        }
        prev.require_full_check_reinit =
            prev.require_full_check_reinit || workload.require_full_check_reinit;
    });
}

pub fn get_and_clear_recheck_workload(
    process_updates: &dyn Fn(bool, &BTreeSet<String>) -> Updates,
    get_forced: &dyn Fn() -> CheckedSet,
) -> (Priority, RecheckWorkload) {
    recheck_fetch(process_updates, get_forced, Priority::Normal);
    let mut guard = RECHECK_ACC.lock().unwrap();
    let workload = guard.take().unwrap_or_else(empty_recheck_workload);
    let RecheckWorkload {
        files_to_prioritize,
        files_to_recheck,
        files_to_force,
        require_full_check_reinit,
    } = workload;
    // if there are any dependencies to force, then we will return them first and leave everything
    // else in the queue for the next recheck.
    let (dependencies_to_force, files_to_force) = files_to_force.partition_dependencies();
    if dependencies_to_force.is_empty() {
        let workload = RecheckWorkload {
            files_to_prioritize,
            files_to_recheck,
            files_to_force,
            require_full_check_reinit,
        };
        *guard = Some(empty_recheck_workload());
        (Priority::Normal, workload)
    } else {
        // include all files_to_recheck in files_to_prioritize, so that we update the dependency
        // graph and merge all known changes.
        let priority_files_to_prioritize =
            files_to_prioritize.dupe().union(files_to_recheck.dupe());
        let priority_workload = RecheckWorkload {
            files_to_force: dependencies_to_force,
            files_to_prioritize: priority_files_to_prioritize,
            files_to_recheck: FlowOrdSet::new(),
            require_full_check_reinit: false,
        };
        let remaining_workload = RecheckWorkload {
            files_to_force,
            files_to_prioritize: FlowOrdSet::new(),
            files_to_recheck,
            require_full_check_reinit,
        };
        *guard = Some(remaining_workload);
        (Priority::Priority, priority_workload)
    }
}

/// [wait_for stream] blocks until a message arrives on [stream]  
fn wait_for_recheck() {
    RECHECK_NOTIFY.1.recv().unwrap();
}

pub fn wait_for_parallelizable_workload() {
    WORKLOAD_STREAM.wait_for_parallelizable_workload();
}

pub fn wait_for_updates_for_recheck(
    process_updates: &dyn Fn(bool, &BTreeSet<String>) -> Updates,
    get_forced: &dyn Fn() -> CheckedSet,
    priority: Priority,
) -> WorkloadChanges {
    loop {
        wait_for_recheck();
        let before_len = with_recheck_acc(|w| {
            (
                w.files_to_prioritize.len(),
                w.files_to_recheck.len(),
                w.files_to_force.cardinal(),
            )
        });
        if recheck_fetch(process_updates, get_forced, priority) {
            let after_len = with_recheck_acc(|w| {
                (
                    w.files_to_prioritize.len(),
                    w.files_to_recheck.len(),
                    w.files_to_force.cardinal(),
                )
            });
            return WorkloadChanges {
                num_files_to_prioritize: after_len.0 - before_len.0,
                num_files_to_recheck: after_len.1 - before_len.1,
                num_files_to_force: after_len.2 - before_len.2,
            };
        }
    }
}

/// Block until any stream receives something
pub fn wait_for_anything(
    _process_updates: &dyn Fn(bool, &BTreeSet<String>) -> Updates,
    _get_forced: &dyn Fn() -> CheckedSet,
) {
    crossbeam::channel::select! {
        recv(WORKLOAD_NOTIFY.1) -> _ => {}
        recv(ENV_UPDATE_NOTIFY.1) -> _ => {}
        recv(RECHECK_NOTIFY.1) -> _ => {}
    }
}
