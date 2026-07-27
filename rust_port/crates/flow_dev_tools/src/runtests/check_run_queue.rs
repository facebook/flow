/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::mpsc;
use std::thread;

use super::TestResult;
use super::TestStatus;

pub(super) struct QueueOpts {
    pub(super) parallelism: usize,
}

pub(super) struct TestJob {
    pub(super) index: usize,
    pub(super) test_dir: PathBuf,
    pub(super) run: Arc<dyn Fn() -> io::Result<TestResult> + Send + Sync>,
}

pub(super) struct CheckRunQueue {
    parallelism: usize,
    jobs: Vec<Arc<TestJob>>,
    results: Vec<Option<TestResult>>,
    running: usize,
    next_job: usize,
    completion_sender: Option<mpsc::Sender<(usize, TestResult)>>,
}

impl CheckRunQueue {
    pub(super) fn new(jobs: Vec<TestJob>, opts: QueueOpts) -> Self {
        let job_count = jobs.len();
        Self {
            parallelism: opts.parallelism,
            jobs: jobs.into_iter().map(Arc::new).collect(),
            results: vec![None; job_count],
            running: 0,
            next_job: 0,
            completion_sender: None,
        }
    }

    pub(super) fn run(mut self) -> mpsc::Receiver<(usize, TestResult)> {
        let (completion_sender, completion_receiver) = mpsc::channel();
        self.completion_sender = Some(completion_sender);

        if self.jobs.is_empty() {
            self.completion_sender.take();
            return completion_receiver;
        }

        // Start initial batch
        let initial_batch = self.parallelism.min(self.jobs.len());
        let queue = Arc::new(Mutex::new(self));
        for _ in 0..initial_batch {
            Self::start_next(&queue);
        }
        completion_receiver
    }

    fn resolve_with_errors(queue: &Arc<Mutex<Self>>) {
        let (completion_sender, missing_results) = {
            let mut queue = queue
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            let completion_sender = queue.completion_sender.take();
            let mut missing_results = Vec::new();
            for index in 0..queue.jobs.len() {
                if queue.results[index].is_none() {
                    let name = queue.jobs[index]
                        .test_dir
                        .file_name()
                        .map_or_else(String::new, |name| name.to_string_lossy().into_owned());
                    missing_results.push((
                        index,
                        TestResult {
                            status: TestStatus::Error,
                            name,
                            diff: None,
                        },
                    ));
                }
            }
            (completion_sender, missing_results)
        };
        if let Some(sender) = completion_sender {
            for result in missing_results {
                if let Err(error) = sender.send(result) {
                    eprintln!("Internal queue error: {error}");
                    break;
                }
            }
        }
    }

    fn settle_job(queue: &Arc<Mutex<Self>>, job: Arc<TestJob>, result: TestResult) {
        let completion_sender = {
            let mut queue = queue
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            if queue.results[job.index].is_none() {
                queue.results[job.index] = Some(result.clone());
            }
            if queue.running > 0 {
                queue.running -= 1;
            }
            queue.completion_sender.clone()
        };
        if let Some(sender) = completion_sender
            && let Err(error) = sender.send((job.index, result))
        {
            eprintln!("Internal queue error: {error}");
        }

        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| Self::start_next(queue)))
            .is_err()
        {
            eprintln!("Internal queue error: Rust queue panicked");
            Self::resolve_with_errors(queue);
            return;
        }
        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| Self::check_done(queue)))
            .is_err()
        {
            eprintln!("Internal queue error: Rust queue panicked");
            Self::resolve_with_errors(queue);
        }
    }

    fn start_next(queue: &Arc<Mutex<Self>>) {
        let job = {
            let mut queue = queue
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            if queue.next_job >= queue.jobs.len() {
                return;
            }

            let job = Arc::clone(&queue.jobs[queue.next_job]);
            queue.next_job += 1;
            queue.running += 1;

            job
        };

        let worker_queue = Arc::clone(queue);
        let worker_job = Arc::clone(&job);
        let test_name = job
            .test_dir
            .file_name()
            .map_or_else(String::new, |name| name.to_string_lossy().into_owned());
        let worker_test_name = test_name.clone();
        let started = thread::Builder::new()
            .name(format!("flow-runtest-{test_name}"))
            .spawn(move || {
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    (worker_job.run)()
                }));
                let settled = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    match result {
                        Ok(Ok(result)) => Self::settle_job(&worker_queue, worker_job, result),
                        Ok(Err(error)) => {
                            eprintln!("Error running test {worker_test_name}: {error}");
                            Self::settle_job(
                                &worker_queue,
                                worker_job,
                                TestResult {
                                    status: TestStatus::Error,
                                    name: worker_test_name,
                                    diff: None,
                                },
                            );
                        }
                        Err(_) => {
                            eprintln!(
                                "Synchronous error running test {worker_test_name}: Rust job panicked"
                            );
                            Self::settle_job(
                                &worker_queue,
                                worker_job,
                                TestResult {
                                    status: TestStatus::Error,
                                    name: worker_test_name,
                                    diff: None,
                                },
                            );
                        }
                    }
                }));
                if settled.is_err() {
                    eprintln!("Internal queue error: Rust queue panicked");
                    Self::resolve_with_errors(&worker_queue);
                }
            });
        if let Err(error) = started {
            eprintln!("Synchronous error running test {test_name}: {error}");
            Self::settle_job(
                queue,
                job,
                TestResult {
                    status: TestStatus::Error,
                    name: test_name,
                    diff: None,
                },
            );
        }
    }

    fn check_done(queue: &Arc<Mutex<Self>>) {
        let completion_sender = {
            let mut queue = queue
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            if queue.running != 0 || queue.next_job < queue.jobs.len() {
                return;
            }

            queue.completion_sender.take()
        };
        drop(completion_sender);
    }
}
