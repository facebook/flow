/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Coordinates command execution with rechecks and Base mutation.

use std::panic::AssertUnwindSafe;
use std::sync::Arc;

use crossbeam::channel;
use crossbeam::channel::Receiver;
use crossbeam::channel::Sender;
use dupe::Dupe;
use flow_check_cache::CheckContentsCache;
use flow_heap::heap_state::CommittedHeap;
use flow_heap::parsing_heaps::ActiveTransaction;

use crate::lsp_prot::ClientId;
use crate::persistent_connection;
use crate::server_env::EnvRef;
use crate::server_env::with_connections;
use crate::server_status;
use crate::workload_stream::ParallelizableWorkload;
use crate::workload_stream::Workload;
use crate::workload_stream::WorkloadOutcome;
use crate::workload_stream::WorkloadStream;

/// A change to the set of persistent clients recorded in the committed `Env`.
///
/// The message carries the client id rather than a closure: the work is a bounded edit of the
/// client list that the executor performs itself, not arbitrary caller-supplied code whose cost it
/// cannot see.
enum ConnectionChange {
    Connected(ClientId),
    Disconnected(ClientId),
}

/// The Base-consistent view from which the recheck thread starts one recheck series.
///
/// This pairs the executor's current `Env` with the last epoch it has published, so the recheck
/// thread never reads executor-owned mutable state directly and can report exactly which pending
/// rechecks its eventual result completes.
pub struct RecheckSnapshot {
    pub env: EnvRef,
    pub completed_recheck_epoch: u64,
}

/// The complete cross-thread protocol understood by the command executor.
///
/// Executor state is deliberately not shared behind a mutex. Commands, recheck transitions, and
/// heap mutation enter through these messages so one event loop establishes their ordering.
enum Control {
    /// Records that a recheck with this epoch must complete before serial commands may resume.
    RecheckPending {
        recheck_epoch: u64,
    },
    /// Starts a recheck and returns the executor's current Base-consistent server state.
    RecheckStarted {
        reply: std::sync::mpsc::Sender<RecheckSnapshot>,
    },
    /// Commits and publishes a new Base and `Env` on the executor thread.
    CommitRecheck {
        commit: Box<dyn FnOnce() -> EnvRef + Send>,
        completed_recheck_epoch: u64,
        reply: std::sync::mpsc::Sender<EnvRef>,
    },
    /// Ends a recheck series by publishing its completed epoch.
    FinishRecheck {
        completed_recheck_epoch: u64,
        reply: std::sync::mpsc::Sender<()>,
    },
    /// Records a persistent client arriving or leaving, for the executor to fold into `Env`.
    ConnectionChanged {
        change: ConnectionChange,
    },
    /// Requests termination of the executor event loop.
    Shutdown,
    /// Runs one heap-collection slice on the executor and reports whether the cycle is complete.
    CollectHeapSlice {
        heap: Arc<CommittedHeap>,
        work: usize,
        reply: std::sync::mpsc::Sender<bool>,
    },
    PushWorkload {
        name: String,
        workload: Workload,
    },
    PushParallelizableWorkload {
        name: String,
        workload: ParallelizableWorkload,
    },
}

/// The cloneable capability through which other server components interact with the orchestrator.
///
/// A handle can enqueue work and perform synchronous recheck/GC rendezvous, but it neither owns the
/// executor thread nor exposes its `Env`, cache, or scheduling state.
#[derive(Clone)]
pub struct ServerOrchestratorHandle {
    control: Sender<Control>,
}

/// The server-startup owner used to wire producers before command execution begins.
///
/// Construction creates the channels and workload queue together. Starting consumes this value,
/// which makes starting the executor more than once unrepresentable without global singleton state.
pub struct ServerOrchestrator {
    handle: ServerOrchestratorHandle,
    executor: CommandExecutor,
}

/// The resources moved onto the dedicated command-execution thread.
///
/// This type is private because callers coordinate through `ServerOrchestratorHandle`; only the
/// executor may run the event loop that mutates command state.
struct CommandExecutor {
    control: Sender<Control>,
    controls: Receiver<Control>,
    workloads: Arc<WorkloadStream>,
}

/// The lifetime owner of a started command executor.
///
/// Keeping the join handle here ties thread lifetime to an ordinary server-owned value. Dropping
/// it requests shutdown and joins the thread, avoiding a detached thread or process-wide singleton.
pub struct RunningServerOrchestrator {
    control: Sender<Control>,
    thread: Option<std::thread::JoinHandle<()>>,
}

impl Drop for RunningServerOrchestrator {
    fn drop(&mut self) {
        match self.control.send(Control::Shutdown) {
            Ok(()) | Err(_) => {}
        }
        if let Some(thread) = self.thread.take()
            && let Err(payload) = thread.join()
        {
            std::panic::resume_unwind(payload);
        }
    }
}

impl ServerOrchestrator {
    pub fn new() -> Self {
        let (control, controls) = channel::unbounded();
        let workloads = Arc::new(WorkloadStream::create());
        let handle = ServerOrchestratorHandle {
            control: control.clone(),
        };
        Self {
            handle,
            executor: CommandExecutor {
                control,
                controls,
                workloads,
            },
        }
    }

    pub fn handle(&self) -> ServerOrchestratorHandle {
        self.handle.clone()
    }

    pub fn start(self, env: EnvRef) -> RunningServerOrchestrator {
        self.executor.start(env)
    }
}

impl Default for ServerOrchestrator {
    fn default() -> Self {
        Self::new()
    }
}

impl CommandExecutor {
    fn start(self, env: EnvRef) -> RunningServerOrchestrator {
        let (started, wait_for_started) = std::sync::mpsc::channel();
        let control = self.control.clone();
        let builder = std::thread::Builder::new().name("flow-command".to_string());
        #[cfg(not(target_arch = "wasm32"))]
        let builder = builder.stack_size(flow_utils_concurrency::thread_pool::DEFAULT_STACK_SIZE);
        let thread = builder
            .spawn(move || self.run_with_panic_handler(env, started))
            .expect("failed to spawn the command executor");
        wait_for_started
            .recv()
            .expect("the command executor should start before the server publishes readiness");
        RunningServerOrchestrator {
            control,
            thread: Some(thread),
        }
    }

    fn run_with_panic_handler(self, env: EnvRef, started: std::sync::mpsc::Sender<()>) {
        if let Err(payload) = std::panic::catch_unwind(AssertUnwindSafe(|| self.run(env, started)))
        {
            let message = payload
                .downcast_ref::<&str>()
                .copied()
                .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
                .unwrap_or("unknown panic");
            flow_hh_logger::error!("Unhandled exception on the command executor: {}", message);
            flow_common_exit_status::exit(flow_common_exit_status::exit_status_for_panic_message(
                message,
            ));
        }
    }
}

impl ServerOrchestratorHandle {
    pub fn begin_recheck(&self) -> RecheckSnapshot {
        let (reply, receiver) = std::sync::mpsc::channel();
        self.control
            .send(Control::RecheckStarted { reply })
            .expect("the command executor should be running");
        receiver
            .recv()
            .expect("the command executor should publish a recheck snapshot")
    }

    pub fn mark_recheck_pending(&self, recheck_epoch: u64) {
        self.control
            .send(Control::RecheckPending { recheck_epoch })
            .expect("the command control channel should stay open");
    }

    pub fn commit_recheck(
        &self,
        commit: impl FnOnce() -> EnvRef + Send + 'static,
        completed_recheck_epoch: u64,
    ) -> EnvRef {
        let (reply, receiver) = std::sync::mpsc::channel();
        self.control
            .send(Control::CommitRecheck {
                commit: Box::new(commit),
                completed_recheck_epoch,
                reply,
            })
            .expect("the command executor should be running");
        receiver
            .recv()
            .expect("the command executor should commit and publish the recheck")
    }

    pub fn finish_recheck(&self, completed_recheck_epoch: u64) {
        let (reply, receiver) = std::sync::mpsc::channel();
        self.control
            .send(Control::FinishRecheck {
                completed_recheck_epoch,
                reply,
            })
            .expect("the command executor should be running");
        receiver
            .recv()
            .expect("the command executor should finish the recheck");
    }

    pub fn client_connected(&self, client_id: ClientId) {
        self.push_connection_change(ConnectionChange::Connected(client_id));
    }

    pub fn client_disconnected(&self, client_id: ClientId) {
        self.push_connection_change(ConnectionChange::Disconnected(client_id));
    }

    fn push_connection_change(&self, change: ConnectionChange) {
        self.control
            .send(Control::ConnectionChanged { change })
            .expect("the command executor should be running");
    }

    pub fn collect_heap_slice(&self, heap: Arc<CommittedHeap>, work: usize) -> bool {
        let (reply, receiver) = std::sync::mpsc::channel();
        self.control
            .send(Control::CollectHeapSlice { heap, work, reply })
            .expect("the command executor should be running");
        receiver
            .recv()
            .expect("the command executor should finish the heap slice")
    }

    pub fn push_workload(&self, name: &str, workload: Workload) {
        self.control
            .send(Control::PushWorkload {
                name: name.to_string(),
                workload,
            })
            .expect("the command executor should be running");
    }

    pub fn push_parallelizable_workload(&self, name: &str, workload: ParallelizableWorkload) {
        self.control
            .send(Control::PushParallelizableWorkload {
                name: name.to_string(),
                workload,
            })
            .expect("the command executor should be running");
    }
}

/// All mutable state over which commands and rechecks require a single ordering authority.
///
/// The command executor alone owns this value. Co-locating the current `Env`, workload scheduling,
/// recheck progress, deferred commands, and Base-dependent cache means their invariants do not rely
/// on independent locks, flags, or cache-generation checks.
struct State {
    env: EnvRef,
    workloads: Arc<WorkloadStream>,
    latest_pending_recheck_epoch: u64,
    completed_recheck_epoch: u64,
    recheck_in_progress: bool,
    deferred_serial: Option<Workload>,
    deferred_parallel: Vec<ParallelizableWorkload>,
    cache: CheckContentsCache,
}

fn should_hold_serial_commands(
    still_rechecking: bool,
    completed_recheck_epoch: u64,
    latest_pending_recheck_epoch: u64,
) -> bool {
    still_rechecking || completed_recheck_epoch < latest_pending_recheck_epoch
}

impl State {
    fn new(env: EnvRef, workloads: Arc<WorkloadStream>) -> Self {
        Self {
            env,
            workloads,
            latest_pending_recheck_epoch: 0,
            completed_recheck_epoch: 0,
            recheck_in_progress: false,
            deferred_serial: None,
            deferred_parallel: Vec::new(),
            cache: CheckContentsCache::new(),
        }
    }

    /// Records a client arriving or leaving straight away, so that `env.connections` is the
    /// authoritative client list at every point the executor yields.
    fn apply_connection_change(&mut self, change: ConnectionChange) {
        let connections = self.env.connections.dupe();
        let connections = match change {
            ConnectionChange::Connected(client_id) => {
                persistent_connection::add_client_to_clients(connections, client_id)
            }
            ConnectionChange::Disconnected(client_id) => {
                persistent_connection::remove_client_from_clients(connections, client_id)
            }
        };
        self.env = with_connections(self.env.dupe(), connections);
    }

    fn handle_control(&mut self, control: Control) {
        match control {
            Control::RecheckPending { recheck_epoch } => {
                assert!(
                    recheck_epoch >= self.latest_pending_recheck_epoch,
                    "recheck epochs must be monotonic"
                );
                self.latest_pending_recheck_epoch = recheck_epoch;
                self.recheck_in_progress = true;
            }
            Control::RecheckStarted { reply } => {
                self.recheck_in_progress = true;
                let snapshot = RecheckSnapshot {
                    env: self.env.dupe(),
                    completed_recheck_epoch: self.completed_recheck_epoch,
                };
                let _result = reply.send(snapshot);
            }
            Control::CommitRecheck {
                commit,
                completed_recheck_epoch,
                reply,
            } => {
                assert!(
                    self.recheck_in_progress,
                    "a recheck commit requires a pending recheck"
                );
                self.cache.clear();
                persistent_connection::clear_type_parse_artifacts_caches();
                // `commit` rebuilds `Env` from the snapshot the recheck was lent, so it carries
                // that snapshot's client list. Re-impose the executor's, which is the current one.
                let connections = self.env.connections.dupe();
                self.env = with_connections(commit(), connections);
                self.publish_recheck(completed_recheck_epoch, true);
                let _result = reply.send(self.env.dupe());
            }
            Control::FinishRecheck {
                completed_recheck_epoch,
                reply,
            } => {
                self.publish_recheck(completed_recheck_epoch, false);
                let _result = reply.send(());
            }
            Control::ConnectionChanged { change } => self.apply_connection_change(change),
            Control::CollectHeapSlice { heap, work, reply } => {
                let compaction = std::cell::Cell::new(None);
                let before_compact = || {
                    crate::monitor_rpc::status_update(server_status::Event::GCStart);
                    compaction.set(Some((heap.heap_size(), std::time::Instant::now())));
                    self.cache.clear();
                    persistent_connection::clear_type_parse_artifacts_caches();
                };
                let done = heap.collect_slice(work, &before_compact);
                if let Some((old_size, start)) = compaction.get() {
                    let new_size = heap.heap_size();
                    let time_taken = start.elapsed().as_secs_f64();
                    if old_size != new_size {
                        flow_hh_logger::info!(
                            "Heap GC: {} bytes before; {} bytes after; in {} seconds",
                            old_size,
                            new_size,
                            time_taken
                        );
                        flow_event_logger::sharedmem_gc_ran(
                            "aggressive",
                            old_size as f64,
                            new_size as f64,
                            time_taken,
                        );
                    }
                }
                let _result = reply.send(done);
            }
            Control::PushWorkload { name, workload } => {
                self.workloads.push(&name, workload);
            }
            Control::PushParallelizableWorkload { name, workload } => {
                self.workloads.push_parallelizable(&name, workload);
            }
            Control::Shutdown => unreachable!("shutdown is handled by the command loop"),
        }
    }

    fn run_workload(&mut self, workload: &mut Workload) -> WorkloadOutcome {
        if (workload.workload_should_be_cancelled)() {
            return WorkloadOutcome::Completed;
        }
        let transaction = ActiveTransaction::new(self.env.heap.dupe());
        (workload.workload_handler)(&self.env, &transaction.handle(), &self.cache)
    }

    fn publish_recheck(&mut self, completed_recheck_epoch: u64, still_rechecking: bool) {
        assert!(
            completed_recheck_epoch >= self.completed_recheck_epoch,
            "completed recheck epochs must be monotonic"
        );
        self.latest_pending_recheck_epoch = self
            .latest_pending_recheck_epoch
            .max(completed_recheck_epoch);
        self.completed_recheck_epoch = completed_recheck_epoch;
        self.recheck_in_progress = should_hold_serial_commands(
            still_rechecking,
            self.completed_recheck_epoch,
            self.latest_pending_recheck_epoch,
        );
        for workload in self.deferred_parallel.drain(..).rev() {
            self.workloads
                .requeue_parallelizable("canceled command", workload);
        }
    }

    fn run_parallelizable(&mut self, workload: &mut ParallelizableWorkload) -> WorkloadOutcome {
        if (workload.parallelizable_workload_should_be_cancelled)() {
            return WorkloadOutcome::Completed;
        }
        let transaction = ActiveTransaction::new(self.env.heap.dupe());
        (workload.parallelizable_workload_handler)(&self.env, &transaction.handle(), &self.cache)
    }

    fn run_next(&mut self) -> bool {
        if !self.recheck_in_progress
            && let Some(mut workload) = self.deferred_serial.take()
        {
            if self.run_workload(&mut workload) == WorkloadOutcome::RetryAfterRecheck {
                self.deferred_serial = Some(workload);
                self.recheck_in_progress = true;
            }
            return true;
        }

        if self.recheck_in_progress {
            let Some(mut workload) = self.workloads.pop_parallelizable() else {
                return false;
            };
            if self.run_parallelizable(&mut workload) == WorkloadOutcome::RetryAfterRecheck {
                self.deferred_parallel.push(workload);
            }
            return true;
        }

        let Some(mut workload) = self.workloads.pop() else {
            return false;
        };
        if self.run_workload(&mut workload) == WorkloadOutcome::RetryAfterRecheck {
            self.deferred_serial = Some(workload);
            self.recheck_in_progress = true;
        }
        true
    }
}

fn run_ready(
    state: &mut State,
    controls: &Receiver<Control>,
    pending_heap_slice: &mut Option<Control>,
) -> bool {
    loop {
        while let Ok(control) = controls.try_recv() {
            match control {
                Control::Shutdown => return false,
                Control::CollectHeapSlice { .. } => {
                    assert!(
                        pending_heap_slice.is_none(),
                        "only one synchronous heap slice may be pending"
                    );
                    *pending_heap_slice = Some(control);
                }
                control => state.handle_control(control),
            }
        }
        if state.run_next() {
            continue;
        }
        if let Some(control) = pending_heap_slice.take() {
            state.handle_control(control);
            continue;
        }
        return true;
    }
}

impl CommandExecutor {
    fn run(self, env: EnvRef, started: std::sync::mpsc::Sender<()>) {
        let mut state = State::new(env, self.workloads);
        let mut pending_heap_slice = None;
        if !run_ready(&mut state, &self.controls, &mut pending_heap_slice) {
            return;
        }
        match started.send(()) {
            Ok(()) => {}
            Err(std::sync::mpsc::SendError(())) => {}
        }
        loop {
            let control = self
                .controls
                .recv()
                .expect("the command control channel should stay open");
            match control {
                Control::Shutdown => return,
                Control::CollectHeapSlice { .. } => pending_heap_slice = Some(control),
                control => state.handle_control(control),
            }
            if !run_ready(&mut state, &self.controls, &mut pending_heap_slice) {
                return;
            }
        }
    }
}
