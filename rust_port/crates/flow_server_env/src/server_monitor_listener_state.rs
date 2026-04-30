/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Mutex;

use crossbeam::channel::Receiver;
use crossbeam::channel::Sender;
use dupe::Dupe;
use flow_common_utils::checked_set::CheckedSet;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;

use crate::monitor_prot::FileWatcherMetadata;
use crate::monitor_prot::empty_file_watcher_metadata;
use crate::monitor_prot::merge_file_watcher_metadata;
use crate::server_env::Env;
use crate::workload_stream::ParallelizableWorkload;
use crate::workload_stream::Workload;
use crate::workload_stream::WorkloadHandler;
use crate::workload_stream::WorkloadStream;

pub type EnvUpdate = Box<dyn FnOnce(Env) -> Env + Send>;

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

static ENV_UPDATE_STREAM: Mutex<Vec<EnvUpdate>> = Mutex::new(Vec::new());

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

pub type FindRefResponseTransformer = Box<
    dyn FnOnce(
            Result<flow_services_references::find_refs_types::FindRefsFound, String>,
        ) -> crate::lsp_prot::ResponseWithMetadata
        + Send,
>;

pub struct FindRefCommand {
    pub request: flow_services_references::find_refs_types::Request,
    pub client_id: crate::lsp_prot::ClientId,
    pub references_to_lsp_response: FindRefResponseTransformer,
}

struct RecheckMsg {
    file_watcher_metadata: Option<FileWatcherMetadata>,
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
    GlobalFindRef {
        def_locs: Vec<Loc>,
        find_ref_command: FindRefCommand,
    },
    DependenciesToPrioritize(FlowOrdSet<FileKey>),
    FilesToReinit {
        files_to_prioritize: FlowOrdSet<FileKey>,
        files_to_recheck: FlowOrdSet<FileKey>,
        files_to_force: CheckedSet,
    },
}

static RECHECK_STREAM: Mutex<Vec<RecheckMsg>> = Mutex::new(Vec::new());

/// Senders for the in-progress recheck-cancel monitors. Each in-progress
/// recheck registers a sender here so a new force-recheck or file-watcher
/// update arriving DURING the recheck can preempt it. This is the Rust port
/// equivalent of OCaml's `Lwt.pick` between the recheck thread and the
/// file-watcher listener inside `run_but_cancel_on_file_changes`. Each push to
/// the recheck stream notifies every registered sender so the corresponding
/// monitor thread can call `worker_cancel::stop_workers()`.
static RECHECK_PUSH_SUBSCRIBERS: Mutex<Vec<Sender<()>>> = Mutex::new(Vec::new());

fn push_recheck_msg_with_metadata(metadata: Option<FileWatcherMetadata>, files: RecheckFiles) {
    RECHECK_STREAM.lock().unwrap().push(RecheckMsg {
        file_watcher_metadata: metadata,
        files,
    });
    RECHECK_NOTIFY.0.send(()).unwrap();
    notify_recheck_push_subscribers();
}

/// Notify every registered recheck-push subscriber that new work arrived. Uses
/// a non-blocking send so a slow or dead subscriber cannot stall the pusher.
/// Disconnected subscribers (whose receiver was dropped without unsubscribing)
/// are removed in the same pass, keeping the subscriber list bounded.
fn notify_recheck_push_subscribers() {
    let mut subscribers = RECHECK_PUSH_SUBSCRIBERS.lock().unwrap();
    subscribers.retain(|sender| match sender.try_send(()) {
        Ok(()) => true,
        Err(crossbeam::channel::TrySendError::Full(())) => true,
        Err(crossbeam::channel::TrySendError::Disconnected(())) => false,
    });
}

/// Subscribe to recheck pushes. The returned receiver fires once per push to
/// the recheck stream (extra pushes while the receiver is full are silently
/// dropped — the receiver only needs to learn that *some* push happened, not
/// how many). When the receiver is dropped the subscriber is removed lazily
/// the next time a push happens, so callers do not need an explicit
/// unsubscribe step.
pub fn subscribe_recheck_pushes() -> Receiver<()> {
    let (tx, rx) = crossbeam::channel::bounded(1);
    RECHECK_PUSH_SUBSCRIBERS.lock().unwrap().push(tx);
    rx
}

fn push_recheck_msg(files: RecheckFiles) {
    push_recheck_msg_with_metadata(None, files);
}

pub fn push_files_to_recheck(changed_files: BTreeSet<String>) {
    push_recheck_msg(RecheckFiles::ChangedFiles(changed_files, false));
}

pub fn push_files_to_recheck_with_metadata(
    metadata: Option<FileWatcherMetadata>,
    changed_files: BTreeSet<String>,
) {
    push_recheck_msg_with_metadata(metadata, RecheckFiles::ChangedFiles(changed_files, false));
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

pub fn push_global_find_ref_request(
    def_locs: Vec<Loc>,
    request: flow_services_references::find_refs_types::Request,
    client_id: crate::lsp_prot::ClientId,
    references_to_lsp_response: FindRefResponseTransformer,
) {
    push_recheck_msg(RecheckFiles::GlobalFindRef {
        def_locs,
        find_ref_command: FindRefCommand {
            request,
            client_id,
            references_to_lsp_response,
        },
    });
}

/// Triggers a recheck of `files`.
/// Call this immediately after a lazy init, where `files` are the files
/// changed since mergebase.
pub fn push_lazy_init(metadata: Option<FileWatcherMetadata>, files: BTreeSet<String>) {
    push_recheck_msg_with_metadata(
        metadata,
        RecheckFiles::FilesToForceFocusedAndRecheck {
            files,
            skip_incompatible: true,
        },
    );
}

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

pub fn wait_for_workload() {
    WORKLOAD_STREAM.wait_for_workload()
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
    pub find_ref_command: Option<FindRefCommand>,
    pub metadata: FileWatcherMetadata,
    pub incompatible_lib_change: bool,
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
        find_ref_command: None,
        metadata: empty_file_watcher_metadata(),
        incompatible_lib_change: false,
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

/// Updates `workload` while leaving it unchanged when there is nothing to do.
fn update(
    workload: &mut RecheckWorkload,
    files_to_prioritize: Option<&FlowOrdSet<FileKey>>,
    files_to_recheck: Option<&FlowOrdSet<FileKey>>,
    files_to_force: Option<&CheckedSet>,
    find_ref_command: Option<FindRefCommand>,
    metadata: Option<&FileWatcherMetadata>,
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
    if find_ref_command.is_some() {
        workload.find_ref_command = find_ref_command;
        changed = true;
    }
    if let Some(new_metadata) = metadata {
        let merged = merge_file_watcher_metadata(new_metadata, &workload.metadata);
        if workload.metadata != merged {
            workload.metadata = merged;
            changed = true;
        }
    }
    changed
}

fn update_to_require_reinit(workload: &mut RecheckWorkload) -> bool {
    if workload.incompatible_lib_change {
        false
    } else {
        workload.incompatible_lib_change = true;
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

/// Processes the messages currently in the recheck stream and returns whether
/// the resulting workload changed.
///
/// The recheck stream gives us files as a set of strings. `process_updates`
/// filters out entries we do not care about and exits on incompatible changes.
///
/// `get_forced` returns the currently forced files. If the recheck stream asks
/// us to focus `foo.js` but it is already focused, then we can ignore it.
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
            let RecheckMsg {
                files,
                file_watcher_metadata,
            } = msg;
            let msg_changed = match files {
                RecheckFiles::ChangedFiles(changed_files, urgent) => {
                    match process_updates(false, &changed_files) {
                        Updates::NormalUpdates(updates) => {
                            if urgent {
                                update(workload, Some(&updates), None, None, None, None)
                            } else {
                                update(workload, None, Some(&updates), None, None, None)
                            }
                        }
                        Updates::RequiredFullCheckReinit(updates) => {
                            let w_changed = if urgent {
                                update(workload, Some(&updates), None, None, None, None)
                            } else {
                                update(workload, None, Some(&updates), None, None, None)
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
                                    .relative_complement(forced_focused.into_inner()),
                            )
                        };
                        let mut files_to_force = CheckedSet::empty();
                        files_to_force.add(Some(focused), None, None);
                        update(
                            workload,
                            None,
                            Some(&updates),
                            Some(&files_to_force),
                            None,
                            None,
                        )
                    }
                    Updates::RequiredFullCheckReinit(updates) => {
                        let focused = {
                            let forced = get_forced();
                            let forced_focused = forced.focused().dupe();
                            FlowOrdSet::from(
                                updates
                                    .dupe()
                                    .into_inner()
                                    .relative_complement(forced_focused.into_inner()),
                            )
                        };
                        let mut files_to_force = CheckedSet::empty();
                        files_to_force.add(Some(focused), None, None);
                        let w_changed = update(
                            workload,
                            None,
                            Some(&updates),
                            Some(&files_to_force),
                            None,
                            None,
                        );
                        let r_changed = update_to_require_reinit(workload);
                        w_changed || r_changed
                    }
                },
                RecheckFiles::GlobalFindRef {
                    def_locs,
                    find_ref_command,
                } => {
                    let files_to_recheck: FlowOrdSet<FileKey> = def_locs
                        .iter()
                        .filter_map(|loc| loc.source())
                        .cloned()
                        .collect();
                    update(
                        workload,
                        None,
                        Some(&files_to_recheck),
                        None,
                        Some(find_ref_command),
                        None,
                    )
                }
                RecheckFiles::DependenciesToPrioritize(dependencies) => {
                    let mut to_prioritize = CheckedSet::empty();
                    to_prioritize.add(None, None, Some(dependencies));
                    let files_to_force = match priority {
                        Priority::Normal => to_prioritize,
                        Priority::Priority => {
                            let forced = get_forced();
                            let mut result = to_prioritize;
                            result.diff(&forced);
                            result
                        }
                    };
                    update(workload, None, None, Some(&files_to_force), None, None)
                }
                RecheckFiles::FilesToReinit {
                    files_to_prioritize,
                    files_to_recheck,
                    files_to_force,
                } => {
                    update(
                        workload,
                        Some(&files_to_prioritize),
                        Some(&files_to_recheck),
                        Some(&files_to_force),
                        None,
                        None,
                    );
                    false
                }
            };
            let metadata_changed = match &file_watcher_metadata {
                None => false,
                Some(metadata) => update(workload, None, None, None, None, Some(metadata)),
            };
            changed = changed || msg_changed || metadata_changed;
        }
    });
    changed
}

pub fn requeue_workload(workload: RecheckWorkload) {
    flow_hh_logger::info!(
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
        prev.find_ref_command = workload.find_ref_command.or(prev.find_ref_command.take());
        prev.metadata = merge_file_watcher_metadata(&prev.metadata, &workload.metadata);
        if prev.incompatible_lib_change || workload.incompatible_lib_change {
            flow_hh_logger::info!(
                "Previous recheck requires restart: {}; new workload requires restart: {}",
                prev.incompatible_lib_change,
                workload.incompatible_lib_change,
            );
        }
        prev.incompatible_lib_change =
            prev.incompatible_lib_change || workload.incompatible_lib_change;
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
        find_ref_command,
        metadata,
        incompatible_lib_change,
    } = workload;
    let (dependencies_to_force, files_to_force) = files_to_force.partition_dependencies();
    if dependencies_to_force.is_empty() {
        let workload = RecheckWorkload {
            files_to_prioritize,
            files_to_recheck,
            files_to_force,
            find_ref_command,
            metadata,
            incompatible_lib_change,
        };
        *guard = Some(empty_recheck_workload());
        (Priority::Normal, workload)
    } else {
        let priority_files_to_prioritize =
            files_to_prioritize.dupe().union(files_to_recheck.dupe());
        let priority_workload = RecheckWorkload {
            files_to_force: dependencies_to_force,
            files_to_prioritize: priority_files_to_prioritize,
            files_to_recheck: FlowOrdSet::new(),
            find_ref_command: None,
            metadata: empty_file_watcher_metadata(),
            incompatible_lib_change: false,
        };
        let remaining_workload = RecheckWorkload {
            files_to_force,
            files_to_prioritize: FlowOrdSet::new(),
            files_to_recheck,
            find_ref_command,
            metadata,
            incompatible_lib_change,
        };
        *guard = Some(remaining_workload);
        (Priority::Priority, priority_workload)
    }
}

/// Blocks until a recheck message arrives.
fn wait_for_recheck() {
    RECHECK_NOTIFY.1.recv().unwrap();
}

pub fn wait_for_parallelizable_workload() {
    WORKLOAD_STREAM.wait_for_parallelizable_workload();
}

pub fn wait_for_parallelizable_workload_or_stop(stop: &std::sync::atomic::AtomicBool) {
    WORKLOAD_STREAM.wait_for_parallelizable_workload_or_stop(stop);
}

pub fn wake_workload_waiters() {
    WORKLOAD_STREAM.wake_waiters();
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
    process_updates: &dyn Fn(bool, &BTreeSet<String>) -> Updates,
    get_forced: &dyn Fn() -> CheckedSet,
) {
    loop {
        crossbeam::channel::select! {
            recv(WORKLOAD_NOTIFY.1) -> _ => return,
            recv(ENV_UPDATE_NOTIFY.1) -> _ => return,
            recv(RECHECK_NOTIFY.1) -> _ => {
                if recheck_fetch(process_updates, get_forced, Priority::Normal) {
                    return;
                }
            }
        }
    }
}
