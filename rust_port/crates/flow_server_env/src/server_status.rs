/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::time::SystemTime;
use std::time::UNIX_EPOCH;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Progress {
    pub total: Option<i32>,
    pub finished: i32,
}

pub type Deadline = f64;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Event {
    /// The server is free  
    Ready,
    /// The server is starting to initialize
    InitStart,
    /// Fetching the saved state is taking a long time
    FetchSavedStateDelay(String),
    ReadSavedState,
    LoadSavedStateProgress(Progress),
    RestoringHeapsStart,
    ParsingProgress(Progress),
    LoadLibrariesStart,
    IndexingProgress(Progress),
    IndexingPostProcess,
    IndexingEnd,
    ResolvingDependenciesProgress,
    CalculatingDependenciesProgress,
    CalculatingDependentsStart,
    CalculatingDependentsEnd,
    MergingProgress(Progress),
    CheckingProgress(Progress),
    CancelingProgress(Progress),
    /// Server's finishing up typechecking or other work  
    FinishingUp,
    /// The server is starting to recheck  
    RecheckStart,
    /// The server is starting to handle an ephemeral/persistent request  
    HandlingRequestStart,
    /// The server is done handling an ephemeral/persistent request
    HandlingRequestEnd,
    /// The server is starting to GC
    GCStart,
    /// The server is collating the errors  
    CollatingErrorsStart,
    /// The server is now blocked waiting for Watchman  
    WatchmanWaitStart(Option<Deadline>),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum TypecheckStatus {
    /// A typecheck's initial state
    StartingTypecheck,
    /// Fetching saved state, when it is taking a while  
    FetchingSavedState(String),
    ReadingSavedState,
    LoadingSavedState(Progress),
    RestoringHeaps,
    LoadingLibraries,
    Parsing(Progress),
    // None means post-processing
    Indexing(Option<Progress>),
    ResolvingDependencies,
    CalculatingDependencies,
    CalculatingDependents,
    Merging(Progress),
    Checking(Progress),
    Canceling(Progress),
    /// We sometimes collate errors during typecheck  
    CollatingErrors,
    /// haven't reached free state yet  
    FinishingTypecheck,
    WaitingForWatchman(Option<Deadline>),
    Unaccounted(String),
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    serde::Serialize,
    serde::Deserialize
)]
pub enum RestartReason {
    ServerOutOfDate,
    OutOfSharedMemory,
    Restart,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    serde::Serialize,
    serde::Deserialize
)]
pub enum TypecheckMode {
    /// Flow is busy starting up  
    Initializing,
    /// Flow is busy rechecking  
    Rechecking,
    /// Flow is busy handling a request
    HandlingRequest,
    /// Same as initializing but with a reason why we restarted
    Restarting(RestartReason),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Status {
    /// The server's initial state  
    StartingUpFlowServer,
    /// Not busy doing something else  
    Free,
    /// Busy doing Flow stuff  
    Typechecking(TypecheckMode, TypecheckStatus),
    /// This one is pretty obvious
    GarbageCollecting,
    /// A bad state caused by transitioning from a good state due to an unexpected event  
    Unknown,
}

pub fn string_of_progress(progress: &Progress) -> String {
    match progress.total {
        None => format!("{}", progress.finished),
        Some(total) => {
            let pct = 100.0 * progress.finished as f64 / (total.max(1)) as f64;
            let pct = if progress.finished < total && pct >= 99.95 {
                99.9
            } else {
                pct
            };
            format!("{}/{} ({:02.1}%)", progress.finished, total, pct)
        }
    }
}

pub enum Emoji {
    ClosedBook,
    Cookie,
    Eyes,
    FileCabinet,
    Ghost,
    Library,
    OpenBook,
    PandaFace,
    Parachute,
    RecyclingSymbol,
    SleepingFace,
    SmilingFaceWithMouthOpen,
    Taco,
    Wastebasket,
    Motorcycle,
    Skier,
    CardIndexDividers,
    QuestionMark,
}

pub fn string_of_emoji(emoji: &Emoji) -> &'static str {
    match emoji {
        Emoji::ClosedBook => "\u{1F4D5}",
        Emoji::Cookie => "\u{1F36A}",
        Emoji::Eyes => "\u{1F440}",
        Emoji::FileCabinet => "\u{1F5C4}",
        Emoji::Ghost => "\u{1F47B}",
        Emoji::Library => "\u{1F3DB}",
        Emoji::OpenBook => "\u{1F4D6}",
        Emoji::PandaFace => "\u{1F43C}",
        Emoji::Parachute => "\u{1FA82}",
        Emoji::RecyclingSymbol => "\u{267B}",
        Emoji::SleepingFace => "\u{1F634}",
        Emoji::SmilingFaceWithMouthOpen => "\u{1F603}",
        Emoji::Taco => "\u{1F32E}",
        Emoji::Wastebasket => "\u{1F5D1}",
        Emoji::Motorcycle => "\u{1F3CD}",
        Emoji::Skier => "\u{26F7}",
        Emoji::CardIndexDividers => "\u{1F5C2}",
        Emoji::QuestionMark => "\u{2753}",
    }
}

pub enum PadEmoji {
    Before,
    After,
}

pub fn render_emoji(use_emoji: bool, pad: PadEmoji, emoji: Emoji) -> String {
    if use_emoji {
        format!(
            "{}{} {}",
            if matches!(pad, PadEmoji::Before) {
                " "
            } else {
                ""
            },
            string_of_emoji(&emoji),
            if matches!(pad, PadEmoji::After) {
                " "
            } else {
                ""
            },
        )
    } else {
        String::new()
    }
}

pub fn string_of_event(event: &Event) -> String {
    match event {
        Event::Ready => "Ready".to_string(),
        Event::InitStart => "Init_start".to_string(),
        Event::FetchSavedStateDelay(message) => {
            format!("Fetch_saved_state_delay {}", message)
        }
        Event::ReadSavedState => "Read_saved_state".to_string(),
        Event::LoadSavedStateProgress(progress) => {
            format!("Load_saved_state_progress {}", string_of_progress(progress))
        }
        Event::RestoringHeapsStart => "Restoring_heaps_start".to_string(),
        Event::ParsingProgress(progress) => {
            format!("Parsing_progress files {}", string_of_progress(progress))
        }
        Event::IndexingProgress(progress) => {
            format!("Indexing_progress {}", string_of_progress(progress))
        }
        Event::IndexingPostProcess => "Indexing_post_process".to_string(),
        Event::IndexingEnd => "Indexing_end".to_string(),
        Event::CalculatingDependenciesProgress => "Calculating_dependencies_progress".to_string(),
        Event::CalculatingDependentsStart => "Calculating_dependents_start".to_string(),
        Event::CalculatingDependentsEnd => "Calculating_dependents_end".to_string(),
        Event::ResolvingDependenciesProgress => "Resolving_dependencies_progress".to_string(),
        Event::MergingProgress(progress) => {
            format!("Merging_progress {}", string_of_progress(progress))
        }
        Event::CheckingProgress(progress) => {
            format!("Checking_progress files {}", string_of_progress(progress))
        }
        Event::CancelingProgress(progress) => {
            format!("Canceling_progress {}", string_of_progress(progress))
        }
        Event::FinishingUp => "Finishing_up".to_string(),
        Event::RecheckStart => "Recheck_start".to_string(),
        Event::HandlingRequestStart => "Handling_request_start".to_string(),
        Event::HandlingRequestEnd => "Handling_request_end".to_string(),
        Event::GCStart => "GC_start".to_string(),
        Event::CollatingErrorsStart => "Collating_errors_start".to_string(),
        Event::WatchmanWaitStart(_deadline) => "Watchman_wait_start".to_string(),
        Event::LoadLibrariesStart => "Loading_libaries_start".to_string(),
    }
}

/// As a general rule, use past tense for status updates that show progress and present perfect
/// progressive for those that don't.
pub fn string_of_typecheck_status(use_emoji: bool, tcs: &TypecheckStatus) -> String {
    match tcs {
        TypecheckStatus::StartingTypecheck => {
            format!(
                "{}starting up",
                render_emoji(use_emoji, PadEmoji::After, Emoji::SleepingFace)
            )
        }
        TypecheckStatus::FetchingSavedState(message) => {
            format!(
                "{}fetching saved state: {}",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Parachute),
                message
            )
        }
        TypecheckStatus::ReadingSavedState => {
            format!(
                "{}reading saved state",
                render_emoji(use_emoji, PadEmoji::After, Emoji::ClosedBook)
            )
        }
        TypecheckStatus::LoadingSavedState(progress) => {
            format!(
                "{}loading saved state {}",
                render_emoji(use_emoji, PadEmoji::After, Emoji::OpenBook),
                string_of_progress(progress)
            )
        }
        TypecheckStatus::RestoringHeaps => {
            format!(
                "{}finalizing saved state heaps",
                render_emoji(use_emoji, PadEmoji::After, Emoji::OpenBook)
            )
        }
        TypecheckStatus::LoadingLibraries => {
            format!(
                "{}loading libraries",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Library)
            )
        }
        TypecheckStatus::Parsing(progress) => {
            format!(
                "{}parsed files {}",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Ghost),
                string_of_progress(progress)
            )
        }
        TypecheckStatus::Indexing(Some(progress)) => {
            format!(
                "{}indexing files {}",
                render_emoji(use_emoji, PadEmoji::After, Emoji::CardIndexDividers),
                string_of_progress(progress)
            )
        }
        TypecheckStatus::Indexing(None) => {
            format!(
                "{}indexing files (post-process)",
                render_emoji(use_emoji, PadEmoji::After, Emoji::CardIndexDividers)
            )
        }
        TypecheckStatus::ResolvingDependencies => {
            format!(
                "{}resolving dependencies",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Taco)
            )
        }
        TypecheckStatus::CalculatingDependencies => {
            format!(
                "{}calculating dependencies",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Taco)
            )
        }
        TypecheckStatus::CalculatingDependents => {
            format!(
                "{}calculating dependents",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Taco)
            )
        }
        TypecheckStatus::Merging(progress) => {
            format!(
                "{}merged module interfaces {}",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Motorcycle),
                string_of_progress(progress)
            )
        }
        TypecheckStatus::Checking(progress) => {
            format!(
                "{}checked files {}",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Skier),
                string_of_progress(progress)
            )
        }
        TypecheckStatus::Canceling(progress) => {
            format!(
                "{}canceling workers {}",
                render_emoji(use_emoji, PadEmoji::After, Emoji::RecyclingSymbol),
                string_of_progress(progress)
            )
        }
        TypecheckStatus::CollatingErrors => {
            format!(
                "{}collating errors",
                render_emoji(use_emoji, PadEmoji::After, Emoji::FileCabinet)
            )
        }
        TypecheckStatus::WaitingForWatchman(deadline) => {
            let timeout = match deadline {
                Some(deadline) => {
                    let now = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap_or_default()
                        .as_secs_f64();
                    format!(
                        " - giving up in {} seconds",
                        0i64.max((deadline - now) as i64)
                    )
                }
                None => String::new(),
            };
            format!(
                "{}waiting for Watchman{}",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Eyes),
                timeout
            )
        }
        TypecheckStatus::FinishingTypecheck => {
            format!(
                "{}finishing up",
                render_emoji(use_emoji, PadEmoji::After, Emoji::Cookie)
            )
        }
        TypecheckStatus::Unaccounted(s) => {
            format!(
                "{}post-{} work",
                render_emoji(use_emoji, PadEmoji::After, Emoji::QuestionMark),
                s
            )
        }
    }
}

pub fn string_of_restart_reason(reason: &RestartReason) -> &'static str {
    match reason {
        RestartReason::ServerOutOfDate => {
            "restarting due to change which cannot be handled incrementally"
        }
        RestartReason::OutOfSharedMemory => "restarting due to running out of shared memory",
        RestartReason::Restart => "restarting to save time",
    }
}

pub fn string_of_status(use_emoji: bool, terse: bool, status: &Status) -> String {
    let status_string = match status {
        Status::StartingUpFlowServer => {
            format!(
                "starting up Flow server{}",
                render_emoji(use_emoji, PadEmoji::Before, Emoji::SleepingFace)
            )
        }
        Status::Free => {
            format!(
                "free{}",
                render_emoji(use_emoji, PadEmoji::Before, Emoji::SmilingFaceWithMouthOpen)
            )
        }
        Status::Typechecking(TypecheckMode::Initializing, tcs) => {
            format!(
                "initializing ({})",
                string_of_typecheck_status(use_emoji, tcs)
            )
        }
        Status::Typechecking(TypecheckMode::Rechecking, tcs) => {
            format!(
                "rechecking ({})",
                string_of_typecheck_status(use_emoji, tcs)
            )
        }
        Status::Typechecking(TypecheckMode::HandlingRequest, tcs) => {
            format!(
                "handling a request ({})",
                string_of_typecheck_status(use_emoji, tcs)
            )
        }
        Status::Typechecking(TypecheckMode::Restarting(reason), tcs) => {
            format!(
                "{} ({})",
                string_of_restart_reason(reason),
                string_of_typecheck_status(use_emoji, tcs)
            )
        }
        Status::GarbageCollecting => {
            format!(
                "garbage collecting shared memory{}",
                render_emoji(use_emoji, PadEmoji::Before, Emoji::Wastebasket)
            )
        }
        Status::Unknown => {
            format!(
                "doing something{}",
                render_emoji(use_emoji, PadEmoji::Before, Emoji::PandaFace)
            )
        }
    };
    format!("{}{}", if terse { "" } else { "Server is " }, status_string)
}

/// Transition function for the status state machine. Given the current status and the event,
/// pick a new status
pub fn update(event: &Event, status: &Status) -> Status {
    match (event, status) {
        (Event::Ready, _) => Status::Free,
        (Event::InitStart, _) => Status::Typechecking(
            TypecheckMode::Initializing,
            TypecheckStatus::StartingTypecheck,
        ),
        (Event::RecheckStart, _) => Status::Typechecking(
            TypecheckMode::Rechecking,
            TypecheckStatus::StartingTypecheck,
        ),
        (Event::FetchSavedStateDelay(message), Status::Typechecking(mode, _)) => {
            Status::Typechecking(
                mode.clone(),
                TypecheckStatus::FetchingSavedState(message.clone()),
            )
        }
        (Event::ReadSavedState, Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::ReadingSavedState)
        }
        (Event::LoadSavedStateProgress(progress), Status::Typechecking(mode, _)) => {
            Status::Typechecking(
                mode.clone(),
                TypecheckStatus::LoadingSavedState(progress.clone()),
            )
        }
        (
            Event::RestoringHeapsStart,
            Status::Typechecking(mode, TypecheckStatus::LoadingSavedState(_)),
        ) => Status::Typechecking(mode.clone(), TypecheckStatus::RestoringHeaps),
        (Event::ParsingProgress(progress), Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::Parsing(progress.clone()))
        }
        (Event::IndexingProgress(progress), Status::Typechecking(mode, _)) => Status::Typechecking(
            mode.clone(),
            TypecheckStatus::Indexing(Some(progress.clone())),
        ),
        (Event::IndexingPostProcess, Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::Indexing(None))
        }
        (Event::IndexingEnd, Status::Typechecking(mode, _)) => Status::Typechecking(
            mode.clone(),
            TypecheckStatus::Unaccounted("Indexing".to_string()),
        ),
        (Event::ResolvingDependenciesProgress, Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::ResolvingDependencies)
        }
        (Event::CalculatingDependenciesProgress, Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::CalculatingDependencies)
        }
        (Event::CalculatingDependentsStart, Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::CalculatingDependents)
        }
        (Event::CalculatingDependentsEnd, Status::Typechecking(mode, _)) => Status::Typechecking(
            mode.clone(),
            TypecheckStatus::Unaccounted("Calculating dependents".to_string()),
        ),
        (Event::MergingProgress(progress), Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::Merging(progress.clone()))
        }
        (Event::CheckingProgress(progress), Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::Checking(progress.clone()))
        }
        (Event::CancelingProgress(progress), Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::Canceling(progress.clone()))
        }
        (Event::CollatingErrorsStart, Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::CollatingErrors)
        }
        (Event::WatchmanWaitStart(deadline), Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::WaitingForWatchman(*deadline))
        }
        (Event::FinishingUp, Status::Typechecking(mode, _)) => {
            Status::Typechecking(mode.clone(), TypecheckStatus::FinishingTypecheck)
        }
        (Event::GCStart, _) => Status::GarbageCollecting,
        (Event::HandlingRequestStart, _) => {
            match status {
                Status::Typechecking(_, _) => {
                    // commands running in parallel with a recheck should not take precedence
                    // over the recheck's status
                    status.clone()
                }
                _ => Status::Typechecking(
                    TypecheckMode::HandlingRequest,
                    TypecheckStatus::StartingTypecheck,
                ),
            }
        }
        (Event::HandlingRequestEnd, _) => {
            match status {
                Status::Typechecking(TypecheckMode::HandlingRequest, _) => {
                    // as above, parallel rechecks take precedence, so only finish handling a
                    // request if that's still the most important thing we're doing.
                    Status::Typechecking(
                        TypecheckMode::HandlingRequest,
                        TypecheckStatus::FinishingTypecheck,
                    )
                }
                _ => status.clone(),
            }
        }
        (Event::LoadLibrariesStart, _) => Status::Typechecking(
            TypecheckMode::Initializing,
            TypecheckStatus::LoadingLibraries,
        ),
        _ => {
            // This is a bad transition. In dev mode, let's blow up since something is wrong. However in
            // production let's soldier on. Usually this means that we forgot to send something like
            // Handling_request_start before we sent a Merging_progress.
            if cfg!(debug_assertions) {
                panic!(
                    "Unexpected status transition from '{}' with event '{}'",
                    string_of_status(false, false, status),
                    string_of_event(event)
                )
            } else {
                // Unknown
                Status::Unknown
            }
        }
    }
}

pub const INITIAL_STATUS: Status = Status::StartingUpFlowServer;

pub fn is_free(status: &Status) -> bool {
    match status {
        Status::Free => true,
        _ => false,
    }
}

/// Returns true iff the transition from old_status to new_status is "significant", which is a
/// pretty arbitrary judgement of how interesting the new status is to a user. Significant
/// transitions are pushed to the user immediately; insignificant transitions are throttled.
pub fn is_significant_transition(old_status: &Status, new_status: &Status) -> bool {
    // If the statuses are literally the same, then the transition is not significant
    old_status != new_status
        && (in_flow_test()
            || match (old_status, new_status) {
                // These are very short transition statuses that aren't important unless
                // they take a long time for some reason.
                (_, Status::Typechecking(_, TypecheckStatus::StartingTypecheck))
                | (_, Status::Typechecking(_, TypecheckStatus::FinishingTypecheck)) => false,
                (
                    Status::Typechecking(old_mode, old_tc_status),
                    Status::Typechecking(new_mode, new_tc_status),
                ) => {
                    // A change in mode is always signifcant
                    old_mode != new_mode
                        || match (old_tc_status, new_tc_status) {
                            // Making progress within parsing, merging or canceling is not significant
                            (TypecheckStatus::Parsing(_), TypecheckStatus::Parsing(_))
                            | (
                                TypecheckStatus::Indexing(Some(_)),
                                TypecheckStatus::Indexing(Some(_)),
                            )
                            | (TypecheckStatus::Indexing(None), TypecheckStatus::Indexing(None))
                            | (TypecheckStatus::Merging(_), TypecheckStatus::Merging(_))
                            | (TypecheckStatus::Checking(_), TypecheckStatus::Checking(_))
                            | (TypecheckStatus::Canceling(_), TypecheckStatus::Canceling(_))
                            | (TypecheckStatus::Unaccounted(_), TypecheckStatus::Unaccounted(_)) => {
                                false
                            }
                            // But changing typechecking status always is significant
                            (_, TypecheckStatus::FetchingSavedState(_))
                            | (_, TypecheckStatus::ReadingSavedState)
                            | (_, TypecheckStatus::LoadingSavedState(_))
                            | (_, TypecheckStatus::RestoringHeaps)
                            | (_, TypecheckStatus::LoadingLibraries)
                            | (_, TypecheckStatus::Parsing(_))
                            | (_, TypecheckStatus::Indexing(_))
                            | (_, TypecheckStatus::ResolvingDependencies)
                            | (_, TypecheckStatus::CalculatingDependencies)
                            | (_, TypecheckStatus::CalculatingDependents)
                            | (_, TypecheckStatus::Merging(_))
                            | (_, TypecheckStatus::Checking(_))
                            | (_, TypecheckStatus::Canceling(_))
                            | (_, TypecheckStatus::WaitingForWatchman(_))
                            | (_, TypecheckStatus::CollatingErrors)
                            | (_, TypecheckStatus::Unaccounted(_)) => true,
                            // also handled above
                            (_, TypecheckStatus::StartingTypecheck)
                            | (_, TypecheckStatus::FinishingTypecheck) => false,
                        }
                }
                // Switching to a completely different status is always significant
                (_, Status::StartingUpFlowServer)
                | (_, Status::Free)
                | (_, Status::Typechecking(_, _))
                | (_, Status::GarbageCollecting)
                | (_, Status::Unknown) => true,
            })
}

pub fn get_progress(status: &Status) -> (Option<String>, Option<i32>, Option<i32>) {
    fn print(progress: &Progress) -> (Option<String>, Option<i32>, Option<i32>) {
        match progress {
            Progress {
                finished,
                total: None,
            } => (Some(format!("{}", finished)), Some(*finished), None),
            Progress {
                finished,
                total: Some(total),
            } => (
                Some(format!("{}/{}", finished, total)),
                Some(*finished),
                Some(*total),
            ),
        }
    }
    match status {
        Status::Typechecking(_, TypecheckStatus::Parsing(progress))
        | Status::Typechecking(_, TypecheckStatus::Merging(progress))
        | Status::Typechecking(_, TypecheckStatus::Checking(progress))
        | Status::Typechecking(_, TypecheckStatus::Canceling(progress)) => print(progress),
        _ => (None, None, None),
    }
}

/// When the server is initializing it will publish statuses that say it is initializing. The
/// monitor might know that the server actually is restarting. This function turns a initializing
/// status into a restarting status
pub fn change_init_to_restart(restart_reason: Option<RestartReason>, status: Status) -> Status {
    match restart_reason {
        None => status,
        Some(restart_reason) => match status {
            Status::Typechecking(TypecheckMode::Initializing, tcs) => {
                Status::Typechecking(TypecheckMode::Restarting(restart_reason), tcs)
            }
            _ => status,
        },
    }
}

fn in_flow_test() -> bool {
    match std::env::var("IN_FLOW_TEST").ok().as_deref() {
        Some("1") | Some("true") => true,
        _ => false,
    }
}
