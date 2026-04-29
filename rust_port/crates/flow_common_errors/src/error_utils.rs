/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::borrow::Cow;
use std::collections::BTreeSet;

use dupe::Dupe;
use flow_common::files;
use flow_lint_settings::lints::LintKind;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::offset_utils::OffsetKind;
use flow_parser::offset_utils::OffsetTable;

use crate::error_codes::ErrorCode;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum InferWarningKind {
    ExportKind,
    OtherKind,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ErrorKind {
    /// An error produced by the parser
    ParseError,
    /// An error produced elsewhere but reported as a parse error
    PseudoParseError,
    InferError,
    InferWarning(InferWarningKind),
    InternalError,
    DuplicateProviderError,
    RecursionLimitError,
    LintError(LintKind),
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseError => write!(f, "ParseError"),
            Self::PseudoParseError => write!(f, "PseudoParseError"),
            Self::InferError => write!(f, "InferError"),
            Self::InferWarning(_) => write!(f, "InferWarning"),
            Self::InternalError => write!(f, "InternalError"),
            Self::DuplicateProviderError => write!(f, "DuplicateProviderError"),
            Self::RecursionLimitError => write!(f, "RecursionLimitError"),
            Self::LintError(lint_kind) => write!(f, "LintError-{}", lint_kind.as_str()),
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum RootKind {
    OperationRoot,
    ShortExplanationRoot,
}

fn string_of_error_code(error_code: Option<ErrorCode>, error_kind: ErrorKind) -> String {
    match error_code {
        Some(code) => code.as_str().to_string(),
        None => error_kind.to_string(),
    }
}

/// internal rep for core info
#[derive(Debug, Clone, PartialEq, Eq)]
enum Message<L> {
    BlameM(L, String),
    CommentM(String),
}

/// simple structure for callers to specify error message content,
/// converted to message internally.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Info<L>(pub L, pub Vec<String>);

/// for extra info, enough structure to do simple tree-shaped output
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InfoTree<L> {
    InfoLeaf(Vec<Info<L>>),
    InfoNode(Vec<Info<L>>, Vec<InfoTree<L>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassicError<L> {
    messages: Vec<Message<L>>,
    extra: Vec<InfoTree<L>>,
    error_codes: Vec<ErrorCode>,
}

use flow_utils_tty as tty;

/// Types and utilities for friendly errors
pub mod friendly {
    use std::collections::BTreeMap;

    use flow_parser::loc::Loc;

    use super::*;

    /// The error message format is designed to render well in all the environments
    /// which a Flow error message appears. This includes:
    ///
    /// - The CLI.
    /// - An IDE.
    /// - A CI job.
    /// - https://flow.org/try
    ///
    /// Some environments can render full blocks of code, like the CLI. Most
    /// environments work best with a sentence of text. Like an IDE or a CI job.
    /// So we optimize our error messages for a single, readable, sentence and
    /// enhance in environments with greater capabilities.
    ///
    /// Messages can have some inline styles. Such as inline code blocks. We also
    /// include references to location in code in our messages. This information
    /// is enhanced in more capable environments instead of degrading in less
    /// capable environments.
    ///
    /// The single message is split into two parts. A "root" and the rest of the
    /// message. If we have a root then some environments (such as the CLI) will
    /// merge errors with the same root cause into a single block.
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        serde::Serialize,
        serde::Deserialize
    )]
    pub(super) struct FriendlyGeneric<L> {
        pub(super) loc: L,
        pub(super) root: Option<ErrorRoot<L>>,
        pub(super) code: Option<ErrorCode>,
        pub(super) message: ErrorMessage<L>,
    }

    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        serde::Serialize,
        serde::Deserialize
    )]
    pub(super) struct ErrorRoot<L> {
        pub(super) root_loc: L,
        pub(super) root_kind: RootKind,
        pub(super) root_message: Message<L>,
    }

    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        serde::Serialize,
        serde::Deserialize
    )]
    pub(super) enum ErrorMessage<L> {
        Normal {
            message: Message<L>,
            frames: Option<Vec<Message<L>>>,
            parent_frames: Vec<(Message<L>, Vec<Message<L>>)>,
            explanations: Option<Vec<Message<L>>>,
        },
        Speculation {
            frames: Vec<Message<L>>,
            explanations: Vec<Message<L>>,
            branches: Vec<(i32, FriendlyGeneric<L>)>,
        },
    }

    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        serde::Serialize,
        serde::Deserialize
    )]
    pub struct Message<L>(pub Vec<MessageFeature<L>>);

    impl<L> std::ops::Deref for Message<L> {
        type Target = Vec<MessageFeature<L>>;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<L> std::ops::DerefMut for Message<L> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }

    impl<L> FromIterator<MessageFeature<L>> for Message<L> {
        fn from_iter<I: IntoIterator<Item = MessageFeature<L>>>(iter: I) -> Self {
            Message(iter.into_iter().collect())
        }
    }

    impl<L> IntoIterator for Message<L> {
        type Item = MessageFeature<L>;
        type IntoIter = std::vec::IntoIter<MessageFeature<L>>;
        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }

    impl<L> From<Vec<MessageFeature<L>>> for Message<L> {
        fn from(v: Vec<MessageFeature<L>>) -> Self {
            Message(v)
        }
    }

    impl<L> Default for Message<L> {
        fn default() -> Self {
            Message(Vec::new())
        }
    }

    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        serde::Serialize,
        serde::Deserialize
    )]
    pub enum MessageFeature<L> {
        Inline(Vec<MessageInline>),
        Reference(Vec<MessageInline>, L),
    }

    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        serde::Serialize,
        serde::Deserialize
    )]
    pub enum MessageInline {
        Text(String),
        Code(String),
    }

    /// The composition of some root error message and a list of associated
    /// error messages. This structure is used in two contexts:
    ///
    /// 1. Grouping errors with the same error_root.
    /// 2. Grouping errors from speculation_branches.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub(super) struct MessageGroup<L> {
        pub(super) group_message: Message<L>,
        pub(super) group_message_nested: GroupMessageNested<L>,
        pub(super) group_message_post: Option<Message<L>>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub(super) enum GroupMessageNested<L> {
        NoNesting,
        StackedErrorNesting(Vec<MessageGroup<L>>),
        SpeculationErrorNesting(Vec<MessageGroup<L>>),
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(super) enum BulletPointKind {
        StackedErrorBulletPoint,
        SpeculationErrorBulletPoint,
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Friendly(FriendlyGeneric<Loc>);

    pub(super) fn bullet_char(unicode: bool, kind: BulletPointKind) -> &'static str {
        // Use unicode for the bullet character if unicode is enabled.
        //
        // http://graphemica.com/%E2%86%B3
        // http://graphemica.com/%E2%80%A2
        if unicode {
            match kind {
                BulletPointKind::StackedErrorBulletPoint => "\u{21B3}", // ↳
                BulletPointKind::SpeculationErrorBulletPoint => "\u{2022}", // •
            }
        } else {
            "-"
        }
    }

    /// Converts a string into a message_inline list. e.g.:
    /// "hello `world`" becomes: [Text "hello ", Code "world"].
    ///
    /// The inverse of string_of_message_inlines.
    fn message_inlines_of_string(s: &str) -> Vec<MessageInline> {
        s.split('`')
            .enumerate()
            .map(|(i, s)| {
                if i % 2 == 0 {
                    MessageInline::Text(s.to_owned())
                } else {
                    MessageInline::Code(s.to_owned())
                }
            })
            .collect()
    }

    /// Converts a string into a message. e.g.:
    /// "hello `world`" becomes a message where "world" is styled as inline code.
    pub fn message_of_string<L>(s: &str) -> Message<L> {
        Message(vec![MessageFeature::Inline(message_inlines_of_string(s))])
    }

    /// Converts a message_inline list into a string. e.g.:
    /// [Text "hello ", Code "world"] becomes: "hello `world`"
    ///
    /// The inverse of message_inlines_of_string.
    pub(super) fn string_of_message_inlines(inlines: &[MessageInline]) -> String {
        let mut message = String::new();
        for inline in inlines {
            match inline {
                MessageInline::Text(text) => message.push_str(text),
                MessageInline::Code(text) => {
                    message.push('`');
                    message.push_str(text);
                    message.push('`');
                }
            }
        }
        message
    }

    // Convenience functions for constructing friendly error messages. e.g.
    //
    //     [ref lower; text " is incompatible with "; ref upper]
    //
    // Is an example of an incompatibility error message.

    pub fn text<L>(s: impl Into<String>) -> MessageFeature<L> {
        MessageFeature::Inline(vec![MessageInline::Text(s.into())])
    }

    pub fn code<L>(s: impl Into<String>) -> MessageFeature<L> {
        MessageFeature::Inline(vec![MessageInline::Code(s.into())])
    }

    fn desc_of_reason_desc_help<L: dupe::Dupe>(
        desc: &flow_common::reason::VirtualReasonDesc<L>,
    ) -> Vec<MessageInline> {
        use flow_common::reason::VirtualReasonDesc;
        use flow_common::reason::string_of_desc;

        let desc = if desc.is_scalar() {
            desc.unwrap()
        } else {
            desc
        };

        match desc {
            VirtualReasonDesc::RCode(code) => vec![MessageInline::Code(code.to_string())],
            _ => message_inlines_of_string(&string_of_desc(desc)),
        }
    }

    fn desc_helper<L: dupe::Dupe>(r: &flow_common::reason::VirtualReason<L>) -> Vec<MessageInline> {
        desc_of_reason_desc_help(r.desc(false))
    }

    pub fn desc<L: dupe::Dupe, R>(r: &flow_common::reason::VirtualReason<L>) -> MessageFeature<R> {
        MessageFeature::Inline(desc_helper(r))
    }

    pub fn desc_of_reason_desc<L: dupe::Dupe, R>(
        desc: &flow_common::reason::VirtualReasonDesc<L>,
    ) -> MessageFeature<R> {
        MessageFeature::Inline(desc_of_reason_desc_help(desc))
    }

    pub fn ref_map<L, F>(
        map_loc: F,
        r: &flow_common::reason::VirtualReason<L>,
    ) -> MessageFeature<Loc>
    where
        L: dupe::Dupe,
        F: Fn(&L) -> Loc,
    {
        let desc = desc_helper(r);
        let loc = match r.annot_loc() {
            Some(loc) => map_loc(loc),
            None => map_loc(r.def_loc()),
        };
        if loc == flow_parser::loc::LOC_NONE {
            MessageFeature::Inline(desc)
        } else {
            MessageFeature::Reference(desc, loc)
        }
    }

    pub fn no_desc_ref_map<L, R, F>(map_loc: F, loc: &L) -> MessageFeature<R>
    where
        F: Fn(&L) -> R,
    {
        MessageFeature::Reference(vec![], map_loc(loc))
    }

    pub fn reference(r: &flow_common::reason::ConcreteReason) -> MessageFeature<Loc> {
        ref_map(|loc| loc.clone(), r)
    }

    pub fn no_desc_ref(loc: &Loc) -> MessageFeature<Loc> {
        no_desc_ref_map(|l: &Loc| l.clone(), loc)
    }

    pub fn hardcoded_string_desc_ref(desc: &str, loc: Loc) -> MessageFeature<Loc> {
        let desc_inlines = message_inlines_of_string(desc);
        if loc == flow_parser::loc::LOC_NONE {
            MessageFeature::Inline(desc_inlines)
        } else {
            MessageFeature::Reference(desc_inlines, loc)
        }
    }

    /// Concatenates a list of messages with a conjunction according to the "rules"
    /// of the English language.
    pub fn conjunction_concat<L>(
        parts: Vec<Message<L>>,
        conjunction: &str,
        limit: Option<usize>,
    ) -> Message<L> {
        let parts = if let Some(limit) = limit {
            let length = parts.len();
            if length <= limit {
                parts
            } else {
                let mut limited_parts: Vec<Message<L>> =
                    parts.into_iter().take(limit - 1).collect();
                let others_text = format!("{} others", length - limit + 1);
                limited_parts.push(Message(vec![MessageFeature::Inline(vec![
                    MessageInline::Text(others_text),
                ])]));
                limited_parts
            }
        } else {
            parts
        };

        let len = parts.len();
        match len {
            0 => Message(vec![]),
            1 => parts.into_iter().next().unwrap(),
            2 => {
                let mut iter = parts.into_iter();
                let mut result = iter.next().unwrap();
                result.push(MessageFeature::Inline(vec![MessageInline::Text(format!(
                    " {} ",
                    conjunction
                ))]));
                result.extend(iter.next().unwrap());
                result
            }
            _ => {
                // Build iteratively: "a, b, c, ... , and z"
                let mut iter = parts.into_iter().peekable();
                let mut result = iter.next().unwrap();
                while let Some(part) = iter.next() {
                    let separator = if iter.peek().is_none() {
                        // Last element - use conjunction
                        format!(", {} ", conjunction)
                    } else {
                        // Not last element - just comma
                        ", ".to_string()
                    };
                    result.push(MessageFeature::Inline(vec![MessageInline::Text(separator)]));
                    result.extend(part);
                }
                result
            }
        }
    }

    // Flattens out the Inline and Text constructors in an error message. Helpful
    // for hiding implementation details in our JSON output.
    pub(super) fn flatten_message<L>(features: Message<L>) -> Message<L> {
        fn loop_inlines(inlines: Vec<MessageInline>) -> Vec<MessageInline> {
            let mut iter = inlines.into_iter();
            let Some(first) = iter.next() else {
                return Vec::new();
            };
            match first {
                MessageInline::Code(_) => {
                    let mut result = vec![first];
                    result.extend(loop_inlines(iter.collect()));
                    result
                }
                MessageInline::Text(text) => {
                    let mut rest = loop_inlines(iter.collect());
                    match rest.first_mut() {
                        None => vec![MessageInline::Text(text)],
                        Some(MessageInline::Text(text2)) => {
                            let mut new_text = text;
                            new_text.push_str(text2);
                            *text2 = new_text;
                            rest
                        }
                        Some(_) => {
                            let mut result = vec![MessageInline::Text(text)];
                            result.append(&mut rest);
                            result
                        }
                    }
                }
            }
        }

        fn loop_features<L>(features: Vec<MessageFeature<L>>) -> Vec<MessageFeature<L>> {
            let mut iter = features.into_iter();
            let Some(first) = iter.next() else {
                return Vec::new();
            };
            match first {
                MessageFeature::Reference(inlines, loc) => {
                    let inlines = loop_inlines(inlines);
                    let mut features = loop_features(iter.collect());
                    let feature = MessageFeature::Reference(inlines, loc);
                    match features.first_mut() {
                        None => vec![feature],
                        Some(MessageFeature::Inline(inlines2)) => {
                            *inlines2 = loop_inlines(std::mem::take(inlines2));
                            let mut result = vec![feature];
                            result.append(&mut features);
                            result
                        }
                        Some(_) => {
                            let mut result = vec![feature];
                            result.append(&mut features);
                            result
                        }
                    }
                }
                MessageFeature::Inline(inlines) => {
                    let mut features = loop_features(iter.collect());
                    match features.first_mut() {
                        None => vec![MessageFeature::Inline(inlines)],
                        Some(MessageFeature::Inline(inlines2)) => {
                            let mut merged = inlines;
                            merged.append(inlines2);
                            *inlines2 = merged;
                            features
                        }
                        Some(_) => {
                            let mut result = vec![MessageFeature::Inline(inlines)];
                            result.append(&mut features);
                            result
                        }
                    }
                }
            }
        }

        let mut features = loop_features(features.0);
        if let Some(MessageFeature::Inline(inlines)) = features.first_mut() {
            *inlines = loop_inlines(std::mem::take(inlines));
        }
        Message(features)
    }

    // Capitalizes the first letter in the message. Does not capitalize code or
    // text in references.
    pub fn capitalize<L>(mut message: Message<L>) -> Message<L> {
        if let Some(MessageFeature::Inline(inlines)) = message.0.first_mut() {
            if let Some(MessageInline::Text(s)) = inlines.first_mut() {
                let mut chars = s.chars();
                if let Some(first) = chars.next() {
                    *s = first.to_ascii_uppercase().to_string() + chars.as_str();
                }
            }
        }
        message
    }

    // Uncapitalizes the first letter in the message. Does not uncapitalize code
    // or text in references.
    fn uncapitalize<L>(mut message: Message<L>) -> Message<L> {
        if let Some(MessageFeature::Inline(inlines)) = message.0.first_mut() {
            if let Some(MessageInline::Text(s)) = inlines.first_mut() {
                let mut chars = s.chars();
                if let Some(first) = chars.next() {
                    *s = first.to_ascii_lowercase().to_string() + chars.as_str();
                }
            }
        }
        message
    }

    // Adds some message to the beginning of a group message.
    fn append_group_message<L>(message: Message<L>, group: MessageGroup<L>) -> MessageGroup<L> {
        let mut new_group_message = message;
        new_group_message.push(text(" "));
        new_group_message.extend(uncapitalize(group.group_message));

        MessageGroup {
            group_message: new_group_message,
            group_message_nested: group.group_message_nested,
            group_message_post: group.group_message_post,
        }
    }

    fn msg_of_error_code_suffix_for_display(
        error_code: Option<crate::error_codes::ErrorCode>,
        error_kind: super::ErrorKind,
    ) -> Message<Loc> {
        Message(vec![
            text(" ["),
            text(super::string_of_error_code(error_code, error_kind)),
            text("]"),
        ])
    }

    fn intersperse_concat<L>(messages: Vec<Message<L>>, sep: Message<L>) -> Message<L>
    where
        L: Dupe,
    {
        let mut result = Message(Vec::new());
        let mut first = true;
        for msg in messages {
            if first {
                first = false;
            } else {
                result.extend(sep.clone());
            }
            result.extend(msg);
        }
        result
    }

    // Creates a message group from the error_message type. If show_all_branches
    // is false then we will hide speculation branches with a lower score. If any
    // speculation branches are hidden then the boolean we return will be true.
    pub(super) fn message_group_of_error(
        show_root: bool,
        show_code: bool,
        show_all_branches: bool,
        unicode: bool,
        error_kind: super::ErrorKind,
        error: FriendlyGeneric<Loc>,
    ) -> (Option<(i32, FriendlyGeneric<Loc>)>, Loc, MessageGroup<Loc>) {
        fn message_of_frames(
            mut frames: Vec<Message<Loc>>,
            acc_frames: Vec<Vec<Message<Loc>>>,
        ) -> Message<Loc> {
            let mut all_acc: Vec<Message<Loc>> = acc_frames
                .into_iter()
                .rev()
                .flat_map(|f| f.into_iter())
                .collect();
            frames.append(&mut all_acc);
            frames.reverse();
            intersperse_concat(frames, Message(vec![text(" of ")]))
        }

        fn message_of_explanations(
            leading_sep: &str,
            sep: &str,
            mut explanations: Vec<Message<Loc>>,
            acc_explanations: Vec<Vec<Message<Loc>>>,
        ) -> Message<Loc> {
            for e in &mut explanations {
                e.push(text("."));
            }
            let mut all_acc: Vec<Message<Loc>> = acc_explanations
                .into_iter()
                .rev()
                .flat_map(|f| f.into_iter())
                .collect();
            explanations.append(&mut all_acc);
            explanations.reverse();
            if explanations.is_empty() {
                return Message(vec![]);
            }
            let result = intersperse_concat(explanations, Message(vec![text(sep)]));
            let mut final_result = Message(vec![text(leading_sep)]);
            final_result.extend(result);
            final_result
        }

        fn partition_into_visible_and_hidden_branches(
            show_all_branches: bool,
            branches: Vec<(i32, FriendlyGeneric<Loc>)>,
        ) -> (
            i32,
            Option<(i32, FriendlyGeneric<Loc>)>,
            Vec<FriendlyGeneric<Loc>>,
        ) {
            let mut high_score = i32::MIN;
            let mut hidden_branches: Option<(i32, FriendlyGeneric<Loc>)> = None;
            let mut acc: Vec<FriendlyGeneric<Loc>> = vec![];

            for (score, error) in branches {
                if show_all_branches {
                    // If we are configured to show all branches then always add our error to acc.
                    acc.push(error);
                } else if score > high_score {
                    // If this message has a better score then throw away all old messages.
                    // We are now hiding some messages.
                    if let Some(old) = acc.pop() {
                        hidden_branches = Some((high_score, old));
                    }
                    acc.clear();
                    high_score = score;
                    acc.push(error);
                } else if score == high_score {
                    // If this message has the same score as our high score then add
                    // it to acc and keep our high score.
                    acc.push(error);
                } else {
                    // If this message has a lower score then our high score we skip
                    // the error. We are now hiding at least one message.
                    match &hidden_branches {
                        None => hidden_branches = Some((score, error)),
                        Some((hidden_score, _)) if score > *hidden_score => {
                            hidden_branches = Some((score, error));
                        }
                        _ => {}
                    }
                }
            }

            acc.reverse();
            (high_score, hidden_branches, acc)
        }

        fn loop_(
            show_root: bool,
            show_code: bool,
            show_all_branches: bool,
            hidden_branches: Option<(i32, FriendlyGeneric<Loc>)>,
            error_code: Option<crate::error_codes::ErrorCode>,
            error_kind: super::ErrorKind,
            in_speculation: bool,
            unicode: bool,
            acc_frames: Vec<Vec<Message<Loc>>>,
            acc_explanations: Vec<Vec<Message<Loc>>>,
            error: FriendlyGeneric<Loc>,
        ) -> (Option<(i32, FriendlyGeneric<Loc>)>, Loc, MessageGroup<Loc>) {
            let FriendlyGeneric {
                loc,
                root,
                code: _,
                message: error_message,
            } = error;

            match error_message {
                ErrorMessage::Normal {
                    message,
                    frames,
                    parent_frames,
                    explanations,
                } => {
                    // If there are no frames then we only want to add the root message (if we
                    // have one) and return. We can safely ignore acc_frames. If a message has
                    // frames set to None then the message is not equipped to handle
                    // extra frames.
                    let no_frames = frames.as_ref().is_none_or(|f| f.is_empty());
                    if no_frames && parent_frames.is_empty() && explanations.is_none() {
                        let mut msg = message;
                        if show_code {
                            msg.extend(msg_of_error_code_suffix_for_display(
                                error_code, error_kind,
                            ));
                        }
                        return (
                            hidden_branches,
                            loc,
                            MessageGroup {
                                group_message: msg,
                                group_message_nested: GroupMessageNested::NoNesting,
                                group_message_post: None,
                            },
                        );
                    }

                    // Create normal error messages. General Case:
                    let explanations = match explanations {
                        Some(expl) => {
                            let (leading_sep, sep) = if in_speculation {
                                ("", "")
                            } else {
                                ("\n", "\n")
                            };
                            message_of_explanations(leading_sep, sep, expl, acc_explanations)
                        }
                        None => Message(Vec::new()),
                    };

                    fn stack_item(
                        unicode: bool,
                        incompatibility_msg: Message<Loc>,
                        frames: Vec<Message<Loc>>,
                    ) -> Message<Loc> {
                        if frames.is_empty() {
                            return incompatibility_msg;
                        }
                        let sep = Message(if unicode {
                            vec![text(" \u{203A} ")] // ›
                        } else {
                            vec![text(" > ")]
                        });
                        let frame_msg = intersperse_concat(frames, sep);

                        let mut result = Message(vec![text("in ")]);
                        result.extend(frame_msg);
                        result.push(text(": "));
                        result.extend(incompatibility_msg);
                        result
                    }

                    let group_message = if parent_frames.is_empty() {
                        let incompatibility_message = match frames {
                            None => message,
                            Some(f) if f.is_empty() => message,
                            Some(f) => stack_item(unicode, message, f),
                        };

                        let mut message = match (show_root, root) {
                            (true, Some(r)) => match r.root_kind {
                                super::RootKind::OperationRoot => {
                                    let mut m = r.root_message;
                                    m.push(text(" because "));
                                    m.extend(incompatibility_message);
                                    m
                                }
                                super::RootKind::ShortExplanationRoot => {
                                    let mut m = r.root_message;
                                    m.push(text(": "));
                                    m.extend(incompatibility_message);
                                    m
                                }
                            },
                            _ => incompatibility_message,
                        };

                        // Finish our error message with a period. But only if frames
                        // is empty! Either way, add the error code at the end
                        message.push(text("."));
                        if show_code {
                            message.extend(msg_of_error_code_suffix_for_display(
                                error_code, error_kind,
                            ));
                        }
                        let group_message_post = if explanations.is_empty() {
                            None
                        } else {
                            Some(explanations)
                        };
                        MessageGroup {
                            group_message: message,
                            group_message_nested: GroupMessageNested::NoNesting,
                            group_message_post,
                        }
                    } else {
                        let frames = frames.expect("frames should exist with parent_frames");

                        let init_group = MessageGroup {
                            group_message: stack_item(unicode, message, frames),
                            group_message_nested: GroupMessageNested::NoNesting,
                            group_message_post: None,
                        };

                        let nested_group = parent_frames.into_iter().fold(
                            init_group,
                            |acc, (incomp_msg, frames)| MessageGroup {
                                group_message: stack_item(unicode, incomp_msg, frames),
                                group_message_nested: GroupMessageNested::StackedErrorNesting(
                                    vec![acc],
                                ),
                                group_message_post: None,
                            },
                        );
                        let mut message = match (show_root, root) {
                            (true, Some(r)) => match r.root_kind {
                                super::RootKind::OperationRoot => {
                                    let mut m = r.root_message;
                                    m.push(text(" because:"));
                                    m
                                }
                                super::RootKind::ShortExplanationRoot => {
                                    let mut m = r.root_message;
                                    m.push(text(":"));
                                    m
                                }
                            },
                            _ => Message(Vec::new()),
                        };
                        if show_code {
                            message.extend(msg_of_error_code_suffix_for_display(
                                error_code, error_kind,
                            ));
                        }
                        let group_message_post = if explanations.is_empty() {
                            None
                        } else {
                            Some(explanations)
                        };
                        MessageGroup {
                            group_message: message,
                            group_message_nested: GroupMessageNested::StackedErrorNesting(vec![
                                nested_group,
                            ]),
                            group_message_post,
                        }
                    };

                    (hidden_branches, loc, group_message)
                }

                // When we have a speculation error, do some work to create a message
                // group. Flatten out nested speculation errors with no frames. Hide
                // frames with low scores. Use a single message_group if we hide all but
                // one branches.
                ErrorMessage::Speculation {
                    frames,
                    explanations,
                    branches,
                } => {
                    // Loop through our speculation branches. We will flatten out relevant
                    // union branches and hide branches with a low score in this loop.
                    let branches = {
                        let mut seen = Vec::new();
                        let mut deduped = Vec::new();
                        for branch in branches {
                            if !seen.contains(&branch) {
                                seen.push(branch.clone());
                                deduped.push(branch);
                            }
                        }
                        deduped
                    };
                    let (_, hidden_branches, mut speculation_errors) =
                        partition_into_visible_and_hidden_branches(show_all_branches, branches);

                    // If there is only one branch in acc and we have a hidden branch,
                    // show that hidden branch as well. If we improve our scoring logic
                    // we can remove this.
                    if hidden_branches.is_some()
                        && speculation_errors.len() == 1
                        && let Some((_, hidden_branch)) = &hidden_branches
                    {
                        speculation_errors.push(hidden_branch.clone());
                    }

                    // When there is only one branch in acc (we had one branch with a
                    // "high score") and this error does not have a root then loop while
                    // adding the frames from this speculation error message.
                    if speculation_errors.len() == 1 && speculation_errors[0].root.is_none() {
                        let spec_error = speculation_errors.into_iter().next().unwrap();
                        let mut acc_frames = acc_frames;
                        acc_frames.push(frames);
                        let mut acc_explanations = acc_explanations;
                        acc_explanations.push(explanations);
                        let modified_error = FriendlyGeneric {
                            loc,
                            root,
                            code: spec_error.code,
                            message: spec_error.message,
                        };

                        return loop_(
                            show_root,
                            show_code,
                            show_all_branches,
                            hidden_branches,
                            error_code,
                            error_kind,
                            in_speculation,
                            unicode,
                            acc_frames,
                            acc_explanations,
                            modified_error,
                        );
                    }

                    // If there were more then one branches with high scores add them all
                    // together in an error message group.
                    let frames_msg = message_of_frames(frames, acc_frames);
                    let explanations_msg =
                        message_of_explanations("", "\n\n", explanations, acc_explanations);

                    let header_message = if (root.is_none() || !show_root) && frames_msg.is_empty()
                    {
                        // If we don't have an error root _and_ we don't have any frames
                        // then use a mock error message. (We should always have a root
                        // for speculation errors in theory, but as long as there are
                        // UnknownUses it's possible we won't get a root.)
                        let mut msg = Message(vec![text("all branches are incompatible:")]);
                        if show_code {
                            msg.extend(msg_of_error_code_suffix_for_display(
                                error_code, error_kind,
                            ));
                        }
                        msg
                    } else {
                        // Otherwise create a message with our frames and optionally the
                        // error message root.
                        let frame_prefix = if frames_msg.is_empty() {
                            Message(vec![])
                        } else {
                            let mut m = Message(vec![text("in ")]);
                            m.extend(frames_msg);
                            m
                        };
                        // Add the root to our error message when we are configured to show the root
                        let mut message = match (show_root, root, frame_prefix.is_empty()) {
                            (true, Some(r), true) => match r.root_kind {
                                super::RootKind::OperationRoot => {
                                    let mut m = r.root_message;
                                    m.push(text(" because"));
                                    m
                                }
                                super::RootKind::ShortExplanationRoot => r.root_message,
                            },
                            (true, Some(r), false) => match r.root_kind {
                                super::RootKind::OperationRoot => {
                                    let mut m = r.root_message;
                                    m.push(text(" because "));
                                    m.extend(frame_prefix);
                                    m
                                }
                                super::RootKind::ShortExplanationRoot => {
                                    let mut m = r.root_message;
                                    m.push(text(". More specifically, "));
                                    m.extend(frame_prefix);
                                    m
                                }
                            },
                            _ => frame_prefix,
                        };
                        // Finish our error message with a colon.
                        message.push(text(":"));
                        if show_code {
                            message.extend(msg_of_error_code_suffix_for_display(
                                error_code, error_kind,
                            ));
                        }
                        message
                    };

                    // Get the message group for all of our speculation errors.
                    let mut group_message_list: Vec<MessageGroup<Loc>> = vec![];
                    let mut hidden_branches = hidden_branches;
                    for spec_error in speculation_errors.into_iter() {
                        let (new_hidden, _, msg_group) = loop_(
                            true,
                            false,
                            show_all_branches,
                            hidden_branches,
                            error_code,
                            error_kind,
                            true,
                            unicode,
                            Vec::new(),
                            Vec::new(),
                            spec_error,
                        );
                        hidden_branches = new_hidden;
                        group_message_list.push(msg_group);
                    }

                    let group_message_list: Vec<MessageGroup<Loc>> = group_message_list
                        .into_iter()
                        .enumerate()
                        .map(|(i, mg)| {
                            let prefix = Message(if i == 0 {
                                vec![text("Either")]
                            } else {
                                vec![text("Or")]
                            });
                            append_group_message(prefix, mg)
                        })
                        .collect();

                    // Handle truncation for large branch counts
                    let (group_message_list, group_message_post) = if !show_all_branches
                        && group_message_list.len() > 50
                    {
                        let truncated: Vec<MessageGroup<Loc>> =
                            group_message_list.into_iter().take(50).collect();
                        let mut post: Message<Loc> = message_of_string(
                            "\nOnly showing the the first 50 branches. To see all branches, re-run Flow with --show-all-branches.",
                        );
                        if !explanations_msg.is_empty() {
                            post.push(text("\n\n"));
                            post.extend(explanations_msg);
                        }
                        (truncated, Some(post))
                    } else {
                        let post = if !explanations_msg.is_empty() {
                            let mut p = Message(vec![text("\n")]);
                            p.extend(explanations_msg);
                            Some(p)
                        } else {
                            None
                        };
                        (group_message_list, post)
                    };

                    (
                        hidden_branches,
                        loc,
                        MessageGroup {
                            group_message: header_message,
                            group_message_nested: GroupMessageNested::SpeculationErrorNesting(
                                group_message_list,
                            ),
                            group_message_post,
                        },
                    )
                }
            }
        }

        let error_code = error.code;
        loop_(
            show_root,
            show_code,
            show_all_branches,
            None,
            error_code,
            error_kind,
            false,
            unicode,
            Vec::new(),
            Vec::new(),
            error,
        )
    }

    // The intermediate fold extract_references uses. Returns both a loc_to_id map
    // and an id_to_loc map. These maps are the inverses of one another. Also
    // returns a transformed message.
    fn extract_references_message_intermediate(
        next_id: i32,
        loc_to_id: BTreeMap<Loc, i32>,
        id_to_loc: BTreeMap<i32, Loc>,
        message: Message<Loc>,
    ) -> (i32, BTreeMap<Loc, i32>, BTreeMap<i32, Loc>, Message<i32>) {
        let (next_id, loc_to_id, id_to_loc, message_vec) = message.0.into_iter().fold(
            (next_id, loc_to_id, id_to_loc, Vec::new()),
            |(next_id, mut loc_to_id, mut id_to_loc, mut acc), message_feature| {
                match message_feature {
                    MessageFeature::Inline(inlines) => {
                        acc.push(MessageFeature::Inline(inlines));
                        (next_id, loc_to_id, id_to_loc, acc)
                    }
                    MessageFeature::Reference(inlines, loc) => match loc_to_id.get(&loc) {
                        Some(&id) => {
                            acc.push(MessageFeature::Reference(inlines, id));
                            (next_id, loc_to_id, id_to_loc, acc)
                        }
                        None => {
                            let id = next_id;
                            loc_to_id.insert(loc.clone(), id);
                            id_to_loc.insert(id, loc);
                            acc.push(MessageFeature::Reference(inlines, id));
                            (next_id + 1, loc_to_id, id_to_loc, acc)
                        }
                    },
                }
            },
        );
        (next_id, loc_to_id, id_to_loc, Message(message_vec))
    }

    // The intermediate fold extract_references uses. Returns both a loc_to_id map
    // and an id_to_loc map. These maps are the inverses of one another. Also
    // returns a transformed message.
    pub(super) fn extract_references_intermediate(
        next_id: i32,
        loc_to_id: BTreeMap<Loc, i32>,
        id_to_loc: BTreeMap<i32, Loc>,
        message_group: MessageGroup<Loc>,
    ) -> (
        i32,
        BTreeMap<Loc, i32>,
        BTreeMap<i32, Loc>,
        MessageGroup<i32>,
    ) {
        let (next_id, loc_to_id, id_to_loc, group_message) =
            extract_references_message_intermediate(
                next_id,
                loc_to_id,
                id_to_loc,
                message_group.group_message,
            );

        let map_group_message_list = |next_id: i32,
                                      loc_to_id: BTreeMap<Loc, i32>,
                                      id_to_loc: BTreeMap<i32, Loc>,
                                      list: Vec<MessageGroup<Loc>>|
         -> (
            i32,
            BTreeMap<Loc, i32>,
            BTreeMap<i32, Loc>,
            Vec<MessageGroup<i32>>,
        ) {
            let (next_id, loc_to_id, id_to_loc, group_message_list) = list.into_iter().fold(
                (next_id, loc_to_id, id_to_loc, Vec::new()),
                |(next_id, loc_to_id, id_to_loc, mut acc), mg| {
                    let (next_id, loc_to_id, id_to_loc, mg) =
                        extract_references_intermediate(next_id, loc_to_id, id_to_loc, mg);
                    acc.push(mg);
                    (next_id, loc_to_id, id_to_loc, acc)
                },
            );
            (next_id, loc_to_id, id_to_loc, group_message_list)
        };

        let (next_id, loc_to_id, id_to_loc, group_message_nested) =
            match message_group.group_message_nested {
                GroupMessageNested::NoNesting => {
                    (next_id, loc_to_id, id_to_loc, GroupMessageNested::NoNesting)
                }
                GroupMessageNested::StackedErrorNesting(l) => {
                    let (next_id, loc_to_id, id_to_loc, list) =
                        map_group_message_list(next_id, loc_to_id, id_to_loc, l);
                    (
                        next_id,
                        loc_to_id,
                        id_to_loc,
                        GroupMessageNested::StackedErrorNesting(list),
                    )
                }
                GroupMessageNested::SpeculationErrorNesting(l) => {
                    let (next_id, loc_to_id, id_to_loc, list) =
                        map_group_message_list(next_id, loc_to_id, id_to_loc, l);
                    (
                        next_id,
                        loc_to_id,
                        id_to_loc,
                        GroupMessageNested::SpeculationErrorNesting(list),
                    )
                }
            };

        let (next_id, loc_to_id, id_to_loc, group_message_post) = match message_group
            .group_message_post
        {
            Some(post) => {
                let (n, l, i, msg) =
                    extract_references_message_intermediate(next_id, loc_to_id, id_to_loc, post);
                (n, l, i, Some(msg))
            }
            None => (next_id, loc_to_id, id_to_loc, None),
        };

        (
            next_id,
            loc_to_id,
            id_to_loc,
            MessageGroup {
                group_message,
                group_message_nested,
                group_message_post,
            },
        )
    }

    // Extracts common location references from a message. In order, each location
    // will be replaced with an integer reference starting at 1. If some reference
    // has the same location as another then they will share an id.
    pub(super) fn extract_references(
        message_group: MessageGroup<Loc>,
    ) -> (BTreeMap<i32, Loc>, MessageGroup<i32>) {
        let (_, _, id_to_loc, message) =
            extract_references_intermediate(1, BTreeMap::new(), BTreeMap::new(), message_group);
        (id_to_loc, message)
    }

    // Turns a group_message back into a single-line message. We do this by adding
    // all the messages together. We don't insert newlines. This is a suboptimal
    // representation of a grouped message, but it works for our purposes.
    fn single_line_message_of_group_message<L: Dupe>(message_group: MessageGroup<L>) -> Message<L> {
        fn loop_collect<L>(acc: &mut Vec<Message<L>>, message_group: MessageGroup<L>) {
            let group_message_list = match message_group.group_message_nested {
                GroupMessageNested::NoNesting => vec![],
                GroupMessageNested::StackedErrorNesting(l)
                | GroupMessageNested::SpeculationErrorNesting(l) => l,
            };
            acc.push(message_group.group_message);
            for group in group_message_list {
                loop_collect(acc, group);
            }
            if let Some(post) = message_group.group_message_post {
                acc.push(post);
            }
        }

        let mut acc = Vec::new();
        loop_collect(&mut acc, message_group);
        intersperse_concat(acc, Message(vec![text(" ")]))
    }

    pub(super) fn indented_message_of_group_message<L>(
        unicode: bool,
        message_group: MessageGroup<L>,
    ) -> Message<L> {
        fn indented<L>(
            indentation: Option<(BulletPointKind, usize)>,
            message: Message<L>,
            unicode: bool,
        ) -> Message<L> {
            match indentation {
                None => message,
                Some((kind, indentation)) => {
                    let pad = format!(
                        "\n{}{} ",
                        " ".repeat(indentation),
                        bullet_char(unicode, kind)
                    );
                    let mut result: Message<L> = Message(vec![text(&pad)]);
                    result.extend(message);
                    result
                }
            }
        }

        fn loop_msg<L>(
            indentation: Option<(BulletPointKind, usize)>,
            mut acc: Vec<Message<L>>,
            message_group: MessageGroup<L>,
            unicode: bool,
        ) -> Vec<Message<L>> {
            acc.push(indented(indentation, message_group.group_message, unicode));

            let acc = match message_group.group_message_nested {
                GroupMessageNested::NoNesting => acc,
                GroupMessageNested::StackedErrorNesting(l) => {
                    let new_indent = Some((
                        BulletPointKind::StackedErrorBulletPoint,
                        indentation.map_or(0, |(_, i)| i) + 1,
                    ));
                    loop_list(new_indent, acc, l, unicode)
                }
                GroupMessageNested::SpeculationErrorNesting(l) => {
                    let new_indent = Some((
                        BulletPointKind::SpeculationErrorBulletPoint,
                        indentation.map_or(0, |(_, i)| i) + 1,
                    ));
                    loop_list(new_indent, acc, l, unicode)
                }
            };

            match message_group.group_message_post {
                Some(post) => {
                    let new_indent =
                        indentation.map(|(_, i)| (BulletPointKind::StackedErrorBulletPoint, i + 1));
                    let mut acc = acc;
                    acc.push(indented(new_indent, post, unicode));
                    acc
                }
                None => acc,
            }
        }

        fn loop_list<L>(
            indentation: Option<(BulletPointKind, usize)>,
            acc: Vec<Message<L>>,
            message_group_list: Vec<MessageGroup<L>>,
            unicode: bool,
        ) -> Vec<Message<L>> {
            message_group_list
                .into_iter()
                .fold(acc, |acc, mg| loop_msg(indentation, acc, mg, unicode))
        }

        let msgs = loop_msg(None, Vec::new(), message_group, unicode);
        msgs.into_iter().flatten().collect()
    }

    /// Converts our friendly error to a classic error message.
    pub(super) fn to_classic(
        error_kind: super::ErrorKind,
        error: FriendlyGeneric<Loc>,
    ) -> super::ClassicError<Loc> {
        let error_code = error.code;
        let (_, loc, message) = message_group_of_error(
            true, true, false,
            /* unicode: Not super important for legacy error format */ false, error_kind,
            error,
        );
        // Extract the references from the message.
        let (references, message) = extract_references(message);
        // We use a basic strategy that concatenates all group messages together.
        // This isn't the most attractive approach, but it works for consumers of
        // the classic format.
        let message = single_line_message_of_group_message(message);
        // Turn the message into a string.
        let message_str = message
            .iter()
            .map(|feature| match feature {
                MessageFeature::Inline(inlines) => string_of_message_inlines(inlines),
                MessageFeature::Reference(inlines, id) => {
                    format!("{} [{}]", string_of_message_inlines(inlines), id)
                }
            })
            .collect::<Vec<_>>()
            .join("");

        let extra = if references.is_empty() {
            Vec::new()
        } else {
            let mut extra_items = vec![super::InfoTree::InfoLeaf(vec![super::Info(
                flow_parser::loc::LOC_NONE,
                vec!["References:".to_string()],
            )])];
            for (id, loc) in references {
                extra_items.push(super::InfoTree::InfoLeaf(vec![super::Info(
                    loc,
                    vec![format!("[{}]", id)],
                )]));
            }
            extra_items
        };

        super::ClassicError {
            messages: vec![super::Message::BlameM(loc, message_str)],
            extra,
            error_codes: error_code.map_or(Vec::new(), |code| vec![code]),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct PrintableError<L>(ErrorKind, friendly::FriendlyGeneric<L>);

fn infos_to_messages<L>(infos: Vec<Info<L>>) -> Vec<Message<L>> {
    fn info_to_messages<L>(info: Info<L>) -> Vec<Message<L>> {
        let Info(loc, msgs) = info;
        if msgs.is_empty() {
            vec![Message::BlameM(loc, String::new())]
        } else {
            let mut result = vec![Message::BlameM(loc, msgs[0].clone())];
            result.extend(msgs.into_iter().skip(1).map(Message::CommentM));
            result
        }
    }
    infos.into_iter().flat_map(info_to_messages).collect()
}

pub fn mk_error<L: Dupe>(
    kind: Option<ErrorKind>,
    root: Option<(L, RootKind, friendly::Message<L>)>,
    frames: Option<Vec<(Option<friendly::Message<L>>, friendly::Message<L>)>>,
    explanations: Option<Vec<friendly::Message<L>>>,
    loc: L,
    error_code: Option<ErrorCode>,
    message: friendly::Message<L>,
) -> PrintableError<L> {
    use friendly::*;
    // Partition frames into inner_most_frames and parent_frames
    let (inner_most_frames, parent_frames) = {
        fn partition_parent_frames<L: Dupe>(
            mut current_stack_incompatibility_msg: Message<L>,
            mut acc_current_stack_frames: Vec<Message<L>>,
            mut acc_processed_stack: Vec<(Message<L>, Vec<Message<L>>)>,
            frames: Vec<(Option<Message<L>>, Message<L>)>,
        ) -> Vec<(Message<L>, Vec<Message<L>>)> {
            for (incompatibility_msg_opt, frame_msg) in frames.into_iter() {
                match incompatibility_msg_opt {
                    None => {
                        acc_current_stack_frames.push(frame_msg);
                    }
                    Some(incompatibility_msg) => {
                        acc_current_stack_frames.push(frame_msg);
                        acc_current_stack_frames.reverse();
                        acc_processed_stack
                            .push((current_stack_incompatibility_msg, acc_current_stack_frames));
                        current_stack_incompatibility_msg = incompatibility_msg;
                        acc_current_stack_frames = Vec::new();
                    }
                }
            }
            acc_current_stack_frames.reverse();
            acc_processed_stack.push((current_stack_incompatibility_msg, acc_current_stack_frames));
            acc_processed_stack
        }

        /// The inner most frames and the parent frames should be treated differently.
        /// Consider the following input of mk_error:
        ///
        /// frames (from outer to inner):
        ///         (None, frame1) > (None, frame2) -> (Some m1, frame3) >
        ///         (None, frame4) > (None, frame5) -> (Some m2, frame6) >
        ///         (None, frame7) > (None, frame8)
        /// message
        ///
        /// We should partition them into
        /// inner_most: message, frame 6 > frame7 > frame8
        /// parent_frames: (frame1 > frame2: m1) > (frame3 > frame4 > frame5: m2)
        ///
        /// Note from the illustrating example above, the inner most frame takes the message
        /// from the param of mk_error, while parent frames' message comes from the most inner
        /// frame of the stack item.
        fn partition_inner_most_and_parent_frames<L: Dupe>(
            mut acc: Vec<Message<L>>,
            frames_inner_to_outer: Vec<(Option<Message<L>>, Message<L>)>,
        ) -> (Option<Vec<Message<L>>>, Vec<(Message<L>, Vec<Message<L>>)>) {
            let mut iter = frames_inner_to_outer.into_iter();
            loop {
                match iter.next() {
                    None => {
                        acc.reverse();
                        return (Some(acc), Vec::new());
                    }
                    Some((None, frame_msg)) => {
                        acc.push(frame_msg);
                    }
                    Some((Some(incompatibility_msg), frame_msg)) => {
                        acc.push(frame_msg);
                        acc.reverse();
                        let rest_frames: Vec<_> = iter.collect();
                        let parent_frames = partition_parent_frames(
                            incompatibility_msg,
                            Vec::new(),
                            Vec::new(),
                            rest_frames,
                        );
                        return (Some(acc), parent_frames);
                    }
                }
            }
        }

        match frames {
            None => (None, Vec::new()),
            Some(mut frames) => {
                frames.reverse();
                if !frames.is_empty() {
                    // For the inner most message, we already have an incompatibility message
                    // (from the message param in mk_error). Therefore, we don't need another
                    // incompatibility message from the frame.
                    frames[0].0 = None;
                    partition_inner_most_and_parent_frames(Vec::new(), frames)
                } else {
                    (None, Vec::new())
                }
            }
        }
    };

    let message = ErrorMessage::Normal {
        message,
        frames: inner_most_frames,
        parent_frames,
        explanations,
    };

    PrintableError(
        kind.unwrap_or(ErrorKind::InferError),
        FriendlyGeneric {
            loc,
            root: root.map(|(root_loc, root_kind, root_message)| ErrorRoot {
                root_loc,
                root_kind,
                root_message,
            }),
            code: error_code,
            message,
        },
    )
}

pub fn mk_speculation_error<L: Dupe>(
    kind: Option<ErrorKind>,
    loc: L,
    root: Option<(L, RootKind, friendly::Message<L>)>,
    frames: Vec<friendly::Message<L>>,
    explanations: Vec<friendly::Message<L>>,
    error_code: Option<ErrorCode>,
    speculation_errors: Vec<(i32, PrintableError<L>)>,
) -> PrintableError<L> {
    use friendly::*;
    let branches: Vec<_> = speculation_errors
        .into_iter()
        .map(|(score, PrintableError(_kind, err))| (score, err))
        .collect();
    PrintableError(
        kind.unwrap_or(ErrorKind::InferError),
        FriendlyGeneric {
            loc,
            root: root.map(|(root_loc, root_kind, root_message)| ErrorRoot {
                root_loc,
                root_kind,
                root_message,
            }),
            code: error_code,
            message: ErrorMessage::Speculation {
                frames,
                explanations,
                branches,
            },
        },
    )
}

pub fn code_of_printable_error<L>(err: &PrintableError<L>) -> Option<ErrorCode> {
    err.1.code
}

fn to_pp(message: &Message<Loc>) -> (&Loc, &String) {
    match message {
        Message::BlameM(loc, s) => (loc, s),
        Message::CommentM(s) => (&flow_parser::loc::LOC_NONE, s),
    }
}

pub type StdinFile = Option<(std::path::PathBuf, String)>;

fn default_style(text: &str) -> (tty::Style, String) {
    (tty::Style::Normal(tty::RawColor::Default), text.to_string())
}

fn error_fragment_style(text: &str) -> (tty::Style, String) {
    (tty::Style::Normal(tty::RawColor::Red), text.to_string())
}

fn warning_fragment_style(text: &str) -> (tty::Style, String) {
    (tty::Style::Normal(tty::RawColor::Yellow), text.to_string())
}

fn dim_style(text: &str) -> (tty::Style, String) {
    (tty::Style::Dim(tty::RawColor::Default), text.to_string())
}

const LIB_PREFIX: &str = "[LIB] ";

fn is_short_lib(filename: &str) -> bool {
    filename.len() > LIB_PREFIX.len() && filename.starts_with(LIB_PREFIX)
}

fn relative_path(strip_root: Option<&std::path::Path>, filename: &str) -> String {
    use std::path::Path;
    if is_short_lib(filename) || Path::new(filename).is_relative() {
        return filename.to_string();
    }
    match strip_root {
        Some(root) => files::relative_path(root, filename),
        None => {
            if let Ok(cwd) = std::env::current_dir() {
                let relname = files::relative_path(&cwd, filename);
                if relname.len() < filename.len() {
                    return relname;
                }
            }
            filename.to_string()
        }
    }
}

fn relative_lib_path(strip_root: Option<&std::path::Path>, filename: &str) -> String {
    use std::path::Path;
    match strip_root {
        Some(root) => {
            let root_str = format!("{}{}", root.to_string_lossy(), std::path::MAIN_SEPARATOR);
            if filename.starts_with(&root_str) {
                relative_path(strip_root, filename)
            } else {
                format!(
                    "<BUILTINS>{}{}",
                    std::path::MAIN_SEPARATOR,
                    Path::new(filename)
                        .file_name()
                        .map(|s| s.to_string_lossy().to_string())
                        .unwrap_or_default()
                )
            }
        }
        None => relative_path(strip_root, filename),
    }
}

/// 0-indexed
///
/// If start + len adds to more lines than exist in the file, then the full len
/// will not be returned.
fn get_lines(start: usize, len: usize, content: &str) -> Vec<&str> {
    if len == 0 {
        return Vec::new();
    }
    let result: Vec<&str> = content.split('\n').skip(start).take(len).collect();
    if result.is_empty() && start == 0 {
        vec![""]
    } else {
        result
    }
}

fn read_file<'a>(stdin_file: &'a StdinFile, filename: &str) -> Option<Cow<'a, str>> {
    if let Some((stdin_path, contents)) = stdin_file {
        if stdin_path.to_string_lossy() == filename {
            return Some(Cow::Borrowed(contents));
        }
    }

    if std::path::Path::new(filename).is_relative() {
        panic!("Expected absolute location, got {}", filename)
    }
    std::fs::read_to_string(filename).ok().map(Cow::Owned)
}

fn get_offset_table_expensive(
    stdin_file: &StdinFile,
    offset_kind: OffsetKind,
    loc: &Loc,
) -> Option<OffsetTable> {
    let filename = file_of_source(loc.source.as_ref())?;
    let content = read_file(stdin_file, &filename)?;
    Some(OffsetTable::make_with_kind(offset_kind, &content))
}

fn read_lines_in_file(
    loc: &Loc,
    filename: Option<&str>,
    stdin_file: &StdinFile,
) -> Option<(String, Vec<String>)> {
    let filename = filename?;
    let content = read_file(stdin_file, filename)?;

    let start_line = loc.start.line as usize;
    let end_line = loc.end.line as usize;

    if start_line == 0 {
        return None;
    }

    let lines = get_lines(start_line - 1, end_line - start_line + 1, &content);
    lines.split_first().map(|(first, rest)| {
        (
            first.to_string(),
            rest.iter().map(|s| s.to_string()).collect(),
        )
    })
}

fn file_of_source(source: Option<&FileKey>) -> Option<String> {
    match source {
        Some(file_key) => {
            let filename = file_key.to_absolute();
            let filename = if is_short_lib(&filename) {
                filename[LIB_PREFIX.len()..].to_string()
            } else {
                filename
            };
            Some(filename)
        }
        None => None,
    }
}

pub fn loc_of_printable_error<L: Dupe>(err: &PrintableError<L>) -> L {
    err.1.loc.dupe()
}

fn loc_of_printable_error_for_compare<L>(err: &PrintableError<L>) -> &L {
    let friendly = &err.1;
    match &friendly.root {
        Some(root) => &root.root_loc,
        None => &friendly.loc,
    }
}

pub fn patch_unsuppressable_error<L: Dupe>(err: PrintableError<L>) -> PrintableError<L> {
    use friendly::*;

    let new_explanation = vec![Message(vec![MessageFeature::Inline(vec![
        MessageInline::Text(
            "Errors with this error code are configured to be unsuppressable".to_string(),
        ),
    ])])];

    let PrintableError(kind, friendly_err) = err;
    let message = match friendly_err.message {
        ErrorMessage::Normal {
            message,
            frames,
            parent_frames,
            explanations,
        } => {
            let explanations = match explanations {
                None => Some(new_explanation),
                Some(mut explanations) => {
                    explanations.extend(new_explanation);
                    Some(explanations)
                }
            };
            ErrorMessage::Normal {
                message,
                frames,
                parent_frames,
                explanations,
            }
        }
        ErrorMessage::Speculation {
            frames,
            mut explanations,
            branches,
        } => {
            explanations.extend(new_explanation);
            ErrorMessage::Speculation {
                frames,
                explanations,
                branches,
            }
        }
    };

    PrintableError(
        kind,
        friendly::FriendlyGeneric {
            loc: friendly_err.loc,
            root: friendly_err.root,
            code: friendly_err.code,
            message,
        },
    )
}

pub fn patch_misplaced_error<L: Dupe>(
    strip_root: Option<&std::path::Path>,
    source_file: &FileKey,
    err: PrintableError<L>,
) -> PrintableError<L> {
    use flow_parser::file_key::FileKeyInner;

    let PrintableError(error_kind, friendly_err) = err;
    let message = match friendly_err.message {
        friendly::ErrorMessage::Normal {
            message,
            frames,
            parent_frames,
            explanations,
        } => {
            let is_lib = matches!(source_file.inner(), FileKeyInner::LibFile(_));
            let filename = source_file.as_str();
            let filename = if is_lib {
                relative_lib_path(strip_root, filename)
            } else {
                relative_path(strip_root, filename)
            };
            let message_feature = friendly::MessageFeature::Inline(vec![
                friendly::MessageInline::Text(
                    ". (FLOW BUG: This is a misplaced error. The original error was raised in file "
                        .to_string(),
                ),
                friendly::MessageInline::Code(filename),
                friendly::MessageInline::Text(")".to_string()),
            ]);
            let mut message = message;
            message.push(message_feature);
            friendly::ErrorMessage::Normal {
                message,
                frames,
                parent_frames,
                explanations,
            }
        }
        friendly::ErrorMessage::Speculation {
            frames,
            explanations,
            branches,
        } => {
            // TODO: this patch does not render well for speculative errors
            friendly::ErrorMessage::Speculation {
                frames,
                explanations,
                branches,
            }
        }
    };
    PrintableError(
        error_kind,
        friendly::FriendlyGeneric {
            loc: friendly_err.loc,
            root: friendly_err.root,
            code: friendly_err.code,
            message,
        },
    )
}

pub fn kind_of_printable_error<L>(err: &PrintableError<L>) -> ErrorKind {
    err.0
}

// TODO: deprecate this in favor of Reason.json_of_loc
pub fn deprecated_json_props_of_loc(
    strip_root: Option<&str>,
    loc: &flow_parser::loc::Loc,
) -> Vec<(String, serde_json::Value)> {
    let file = match &loc.source {
        Some(x) => serde_json::Value::String(flow_common::reason::string_of_source(strip_root, x)),
        None => serde_json::Value::String(String::new()),
        // TODO: return serde_json::Value::Null
    };
    vec![
        ("path".to_string(), file),
        ("line".to_string(), serde_json::json!(loc.start.line)),
        ("endline".to_string(), serde_json::json!(loc.end.line)),
        ("start".to_string(), serde_json::json!(loc.start.column + 1)),
        ("end".to_string(), serde_json::json!(loc.end.column)),
    ]
}

// first reason's position, then second reason's position, etc.; if all
// positions match then first message, then second message, etc.
//
// for friendly errors check the location, docs slug, and then message.
fn compare_printable_error<L, F>(
    compare_loc: &F,
    err1: &PrintableError<L>,
    err2: &PrintableError<L>,
) -> std::cmp::Ordering
where
    F: Fn(&L, &L) -> std::cmp::Ordering,
    L: Dupe,
{
    use std::cmp::Ordering;

    // show internal errors first, then duplicate provider errors, then parse
    // errors, then recursion limit errors. then both infer warnings and errors
    // at the same priority. then lint errors
    fn order_of_kind(kind: &ErrorKind) -> i32 {
        match kind {
            ErrorKind::InternalError => 1,
            ErrorKind::DuplicateProviderError => 2,
            ErrorKind::ParseError => 3,
            ErrorKind::PseudoParseError => 3,
            ErrorKind::RecursionLimitError => 4,
            ErrorKind::InferError => 5,
            ErrorKind::InferWarning(_) => 5,
            ErrorKind::LintError(_) => 6,
        }
    }

    fn kind_cmp(k1: &ErrorKind, k2: &ErrorKind) -> Ordering {
        order_of_kind(k1).cmp(&order_of_kind(k2))
    }

    fn compare_lists<T, F>(f: F, list1: &[T], list2: &[T]) -> Ordering
    where
        F: Fn(&T, &T) -> Ordering,
    {
        let mut iter1 = list1.iter();
        let mut iter2 = list2.iter();
        loop {
            match (iter1.next(), iter2.next()) {
                (None, None) => return Ordering::Equal,
                (None, Some(_)) => return Ordering::Less,
                (Some(_), None) => return Ordering::Greater,
                (Some(h1), Some(h2)) => {
                    let k = f(h1, h2);
                    if k != Ordering::Equal {
                        return k;
                    }
                }
            }
        }
    }

    fn compare_option<T, F>(f: F, o1: &Option<T>, o2: &Option<T>) -> Ordering
    where
        F: Fn(&T, &T) -> Ordering,
    {
        match (o1, o2) {
            (Some(x1), Some(x2)) => f(x1, x2),
            (Some(_), None) => Ordering::Greater,
            (None, Some(_)) => Ordering::Less,
            (None, None) => Ordering::Equal,
        }
    }

    fn compare_message_inline(
        m1: &friendly::MessageInline,
        m2: &friendly::MessageInline,
    ) -> Ordering {
        use friendly::MessageInline;
        match (m1, m2) {
            (MessageInline::Text(s1), MessageInline::Text(s2)) => s1.cmp(s2),
            (MessageInline::Text(_), MessageInline::Code(_)) => Ordering::Greater,
            (MessageInline::Code(_), MessageInline::Text(_)) => Ordering::Less,
            (MessageInline::Code(s1), MessageInline::Code(s2)) => s1.cmp(s2),
        }
    }

    fn compare_message_feature<L, F>(
        compare_loc: &F,
        m1: &friendly::MessageFeature<L>,
        m2: &friendly::MessageFeature<L>,
    ) -> Ordering
    where
        F: Fn(&L, &L) -> Ordering,
    {
        use friendly::MessageFeature;
        match (m1, m2) {
            (MessageFeature::Inline(m1), MessageFeature::Inline(m2)) => {
                compare_lists(compare_message_inline, m1, m2)
            }
            (MessageFeature::Inline(_), MessageFeature::Reference(_, _)) => Ordering::Greater,
            (MessageFeature::Reference(_, _), MessageFeature::Inline(_)) => Ordering::Less,
            (MessageFeature::Reference(m1, loc1), MessageFeature::Reference(m2, loc2)) => {
                let k = compare_loc(loc1, loc2);
                if k == Ordering::Equal {
                    compare_lists(compare_message_inline, m1, m2)
                } else {
                    k
                }
            }
        }
    }

    fn compare_friendly_message<L, F>(
        compare_loc: &F,
        m1: &friendly::ErrorMessage<L>,
        m2: &friendly::ErrorMessage<L>,
    ) -> Ordering
    where
        F: Fn(&L, &L) -> Ordering,
        L: Dupe,
    {
        use friendly::ErrorMessage;

        fn compare_parent_frame<L, F>(
            compare_loc: &F,
            pf1: &(friendly::Message<L>, Vec<friendly::Message<L>>),
            pf2: &(friendly::Message<L>, Vec<friendly::Message<L>>),
        ) -> Ordering
        where
            F: Fn(&L, &L) -> Ordering,
        {
            let (m1, frame1) = pf1;
            let (m2, frame2) = pf2;
            let k = compare_lists(
                |a, b| compare_message_feature(compare_loc, a, b),
                &m1.0,
                &m2.0,
            );
            if k == Ordering::Equal {
                compare_lists(
                    |f1, f2| {
                        compare_lists(
                            |a, b| compare_message_feature(compare_loc, a, b),
                            &f1.0,
                            &f2.0,
                        )
                    },
                    frame1,
                    frame2,
                )
            } else {
                k
            }
        }

        match (m1, m2) {
            (
                ErrorMessage::Normal {
                    frames: fs1,
                    explanations: ex1,
                    parent_frames: pf1,
                    message: m1,
                },
                ErrorMessage::Normal {
                    frames: fs2,
                    explanations: ex2,
                    parent_frames: pf2,
                    message: m2,
                },
            ) => {
                let k = compare_option(
                    |v1, v2| {
                        compare_lists(
                            |m1, m2| {
                                compare_lists(
                                    |a, b| compare_message_feature(compare_loc, a, b),
                                    &m1.0,
                                    &m2.0,
                                )
                            },
                            v1,
                            v2,
                        )
                    },
                    fs1,
                    fs2,
                );
                if k == Ordering::Equal {
                    let k = compare_option(
                        |v1, v2| {
                            compare_lists(
                                |m1, m2| {
                                    compare_lists(
                                        |a, b| compare_message_feature(compare_loc, a, b),
                                        &m1.0,
                                        &m2.0,
                                    )
                                },
                                v1,
                                v2,
                            )
                        },
                        ex1,
                        ex2,
                    );
                    if k == Ordering::Equal {
                        let k = compare_lists(
                            |a, b| compare_message_feature(compare_loc, a, b),
                            &m1.0,
                            &m2.0,
                        );
                        if k == Ordering::Equal {
                            compare_lists(
                                |pf1, pf2| compare_parent_frame(compare_loc, pf1, pf2),
                                pf1,
                                pf2,
                            )
                        } else {
                            k
                        }
                    } else {
                        k
                    }
                } else {
                    k
                }
            }
            (ErrorMessage::Normal { .. }, ErrorMessage::Speculation { .. }) => Ordering::Less,
            (ErrorMessage::Speculation { .. }, ErrorMessage::Normal { .. }) => Ordering::Greater,
            (
                ErrorMessage::Speculation {
                    frames: fs1,
                    explanations: ex1,
                    branches: b1,
                },
                ErrorMessage::Speculation {
                    frames: fs2,
                    explanations: ex2,
                    branches: b2,
                },
            ) => {
                let k = compare_lists(
                    |m1, m2| {
                        compare_lists(
                            |a, b| compare_message_feature(compare_loc, a, b),
                            &m1.0,
                            &m2.0,
                        )
                    },
                    fs1,
                    fs2,
                );
                if k == Ordering::Equal {
                    let k = compare_lists(
                        |m1, m2| {
                            compare_lists(
                                |a, b| compare_message_feature(compare_loc, a, b),
                                &m1.0,
                                &m2.0,
                            )
                        },
                        ex1,
                        ex2,
                    );
                    if k == Ordering::Equal {
                        let k = b1.len().cmp(&b2.len());
                        if k == Ordering::Equal {
                            compare_lists(
                                |(_, err1), (_, err2)| {
                                    compare_printable_error(
                                        compare_loc,
                                        &PrintableError(ErrorKind::InferError, err1.clone()),
                                        &PrintableError(ErrorKind::InferError, err2.clone()),
                                    )
                                },
                                b1,
                                b2,
                            )
                        } else {
                            k
                        }
                    } else {
                        k
                    }
                } else {
                    k
                }
            }
        }
    }

    let loc1 = loc_of_printable_error_for_compare(err1);
    let loc2 = loc_of_printable_error_for_compare(err2);

    let (k1, err1) = (&err1.0, &err1.1);
    let (k2, err2) = (&err2.0, &err2.1);

    let k = compare_loc(loc1, loc2);
    if k == Ordering::Equal {
        let k = kind_cmp(k1, k2);
        if k == Ordering::Equal {
            match (&err1.root, &err2.root) {
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (
                    Some(friendly::ErrorRoot {
                        root_message: rm1, ..
                    }),
                    Some(friendly::ErrorRoot {
                        root_message: rm2, ..
                    }),
                ) => {
                    let k = compare_lists(
                        |a, b| compare_message_feature(compare_loc, a, b),
                        &rm1.0,
                        &rm2.0,
                    );
                    if k == Ordering::Equal {
                        let k = compare_loc(&err1.loc, &err2.loc);
                        if k == Ordering::Equal {
                            let k =
                                compare_friendly_message(compare_loc, &err1.message, &err2.message);
                            if k == Ordering::Equal {
                                err1.code.cmp(&err2.code)
                            } else {
                                k
                            }
                        } else {
                            k
                        }
                    } else {
                        k
                    }
                }
                (None, None) => {
                    let k = compare_friendly_message(compare_loc, &err1.message, &err2.message);
                    if k == Ordering::Equal {
                        err1.code.cmp(&err2.code)
                    } else {
                        k
                    }
                }
            }
        } else {
            k
        }
    } else {
        k
    }
}

impl PartialOrd for PrintableError<Loc> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PrintableError<Loc> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        compare_printable_error(&Loc::cmp, self, other)
    }
}

// ============================================================================
// ConcreteLocPrintableErrorSet
// ============================================================================

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ConcreteLocPrintableErrorSet(BTreeSet<PrintableError<Loc>>);

impl ConcreteLocPrintableErrorSet {
    pub fn new() -> Self {
        Self(BTreeSet::new())
    }

    pub fn empty() -> Self {
        Self::new()
    }

    #[allow(dead_code)]
    pub fn add(&mut self, error: PrintableError<Loc>) {
        self.0.insert(error);
    }

    pub fn union(&mut self, other: &Self) {
        for error in &other.0 {
            self.0.insert(error.clone());
        }
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn cardinal(&self) -> usize {
        self.0.len()
    }

    pub fn exists<F>(&self, f: F) -> bool
    where
        F: Fn(&PrintableError<Loc>) -> bool,
    {
        self.0.iter().any(f)
    }

    pub fn iter(&self) -> impl Iterator<Item = &PrintableError<Loc>> {
        self.0.iter()
    }

    pub fn diff(&self, other: &Self) -> Self {
        let mut result = ConcreteLocPrintableErrorSet::new();
        for error in &self.0 {
            if !other.0.contains(error) {
                result.0.insert(error.clone());
            }
        }
        result
    }
}

impl Default for ConcreteLocPrintableErrorSet {
    fn default() -> Self {
        Self::new()
    }
}

type ErrorGroup<L> = (ErrorKind, friendly::FriendlyGeneric<L>);

/// Folds a PrintableErrorSet into a grouped list.
fn collect_errors_into_groups(
    max: Option<usize>,
    set: &ConcreteLocPrintableErrorSet,
) -> Vec<ErrorGroup<Loc>> {
    let result: Result<Vec<_>, Vec<_>> = set.0.iter().enumerate().try_fold(
        Vec::new(),
        |mut acc, (n, PrintableError(kind, error))| {
            let omit = max.is_some_and(|max_val| max_val <= n);
            if omit {
                Err(acc)
            } else {
                acc.push((*kind, error.clone()));
                Ok(acc)
            }
        },
    );
    match result {
        Ok(acc) => acc,
        Err(acc) => acc,
    }
}

/// Human readable output
pub mod cli_output {
    use std::collections::BTreeMap;
    use std::collections::VecDeque;

    use dupe::IterDupedExt;
    use flow_lint_settings::severity::Severity;
    use flow_parser::file_key::FileKey;
    use flow_parser::loc::Position;

    use super::*;

    /// Rendering mode for CLI output
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum RenderingMode {
        CliColorAlways,
        CliColorNever,
        CliColorAuto,
        IdeDetailedError,
    }

    /// Error output flags
    #[derive(Debug, Clone)]
    pub struct ErrorFlags {
        pub rendering_mode: RenderingMode,
        pub include_warnings: bool,
        /// This has to do with the exit code, which is not controlled by this module,
        /// but it's convenient to keep the flags about errors co-located
        pub max_warnings: Option<i32>,
        pub one_line: bool,
        pub list_files: bool,
        pub show_all_errors: bool,
        pub show_all_branches: bool,
        pub unicode: bool,
        pub message_width: i32,
    }

    /// Get the style function for a given severity
    /// OCaml: let severity_fragment_style = function
    fn severity_fragment_style(severity: Severity) -> fn(&str) -> (tty::Style, String) {
        match severity {
            Severity::Err => error_fragment_style,
            Severity::Warn => warning_fragment_style,
            Severity::Off => {
                panic!("CLI output is only called with warnings and errors.")
            }
        }
    }

    fn remove_newlines(styled: &(tty::Style, String)) -> (tty::Style, String) {
        let (color, text) = styled;
        (*color, text.replace('\n', "\\n"))
    }

    fn color_mode_of_rendering_mode(mode: RenderingMode) -> tty::ColorMode {
        match mode {
            RenderingMode::IdeDetailedError | RenderingMode::CliColorAlways => {
                tty::ColorMode::ColorAlways
            }
            RenderingMode::CliColorNever => tty::ColorMode::ColorNever,
            RenderingMode::CliColorAuto => tty::ColorMode::ColorAuto,
        }
    }

    fn should_color(rendering_mode: RenderingMode) -> bool {
        tty::should_color(color_mode_of_rendering_mode(rendering_mode))
    }

    /// ==========================
    /// Full Terminal Width Header
    /// ==========================
    ///
    /// The header will always be the length of flags.message_width which in turn
    /// is the lesser of the terminal length and 120 characters.
    fn print_header_friendly(
        strip_root: Option<&std::path::Path>,
        flags: &ErrorFlags,
        severity: Severity,
        loc: &flow_parser::loc::Loc,
    ) -> Vec<(tty::Style, String)> {
        use flow_parser::file_key::FileKeyInner;

        let severity_style = severity_fragment_style(severity);
        let severity_name = match severity {
            Severity::Err => "Error",
            Severity::Warn => "Warning",
            Severity::Off => panic!("unreachable"),
        };

        let horizontal_line_length = flags.message_width as usize - (severity_name.len() + 1);

        let filename = {
            let line = loc.start.line;
            let column = loc.start.column;
            let pos = format!(":{}:{}", line, column + 1);
            match &loc.source {
                Some(file_key) => match file_key.inner() {
                    FileKeyInner::LibFile(_) => {
                        relative_lib_path(strip_root, &file_key.to_absolute()) + &pos
                    }
                    FileKeyInner::SourceFile(_)
                    | FileKeyInner::JsonFile(_)
                    | FileKeyInner::ResourceFile(_) => {
                        relative_path(strip_root, &file_key.to_absolute()) + &pos
                    }
                },
                None => String::new(),
            }
        };

        // If the filename is longer then the remaining horizontal line length we
        // put the filename on a new line. Otherwise the filename eats some of the
        // horizontal line space.
        //
        // We put two spaces of padding between the horizontal line and the
        // filename. This looks better in some fonts.
        let filename_with_padding_length = filename.len() + 1;
        let filename_on_newline = filename_with_padding_length + 1 > horizontal_line_length;
        let horizontal_line_length = if filename_on_newline {
            horizontal_line_length
        } else {
            horizontal_line_length - filename_with_padding_length
        };

        // The horizontal line which fills the space in our error message header
        // will be made out of [U+2508] characters when unicode is enabled.
        // In UTF-8 this character is encoded with three code points.
        let horizontal_line = if flags.unicode {
            // Build the unicode horizontal line (U+2508 = ┈)
            let mut line = String::with_capacity(horizontal_line_length * 3);
            for _ in 0..horizontal_line_length {
                line.push('\u{2508}');
            }
            line
        } else {
            "-".repeat(horizontal_line_length)
        };

        // Construct the header by appending the constituent pieces.
        let separator = if filename_on_newline { "\n" } else { " " };
        let header_text = format!(
            "{} {}{}{}",
            severity_name, horizontal_line, separator, filename
        );

        vec![severity_style(&header_text), default_style("\n")]
    }

    /// Tag kind for reference layout
    #[derive(Debug, Clone)]
    enum TagKind {
        Open(Position),
        Close,
    }

    #[derive(Debug, Clone, Default)]
    struct Tags(BTreeMap<FileKey, Vec<(Position, i32, TagKind)>>);

    /// See the definition of update_colors for why we need this cyclic type.
    #[derive(Default, Debug, Clone)]
    struct Opened(BTreeMap<i32, Opened>);

    /// Color type for reference layout algorithm
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub(super) enum Color<A> {
        /// Rank based color
        Color(i32),
        /// Custom color passed into the layout_references algorithm
        CustomColor(A),
    }

    /// ==========================
    /// Reference Layout Algorithm
    /// ==========================
    ///
    /// In our error messages, we print a code frame and the code frame may have
    /// any number of overlapping references. These overlapping references are
    /// colored in such a way where the smallest location always has the
    /// same color. For instance, consider the following source text diagram:
    ///
    /// ```text
    ///     Source Text  >  abcdefghijklmnopqrstuvwxyz0123456789
    ///     Coloration   >    XXOOOOXXX  XXXXOOOOOOO     OOOOO
    ///     Reference 1  >    |-------|
    ///     Reference 2  >      |--|
    ///     Reference 3  >               |------|
    ///     Reference 4  >                   |-----|
    ///     Reference 5  >                               |---|
    /// ```
    ///
    /// Reference 1 completely contains reference 2. References 3 and 4 overlap.
    /// Reference 5 is completely on its own.
    ///
    /// The "Coloration" row depicts how we would like to "paint" the source text
    /// in our design for error messages. In the "Coloration" row: spaces represent
    /// unpainted space, O's represent some first color, and X's represent some
    /// second color.
    ///
    /// Notice how reference 2 and reference 5 share the same color. "O". This is
    /// because, in our design, the references with the smallest contained area
    /// always share the same color. The references which contain them have the
    /// second color, "X".
    ///
    /// Notice how in references 3 and 4, one does not contain another. Instead the
    /// two references overlap. This is not common in Flow's error messages.
    /// However, we still need reasonable support for this case in our algorithm.
    /// For implementation convenience, we choose to give the reference which comes
    /// second the "O" color and the reference which comes first the "X" color.
    ///
    /// This example only demonstrates two colors. "O" and "X". However, in
    /// principle there could be any number of overlapping references.
    ///
    /// NOTE: Our current algorithm is not that efficient. There is a lot of room
    /// for optimization. For instance, a binary tree may be used instead of a list
    /// for the variable "tags". Since the number of references in a single file is
    /// almost always under 100, the implementation currently prefers legibility
    /// and correctness to efficiency.
    ///
    /// ## Inputs and Outputs
    ///
    /// We take in a map of reference ids to reference locations.
    ///
    /// We return both:
    /// - A map of reference ids to reference colors.
    /// - A map of filenames to lists of open/close tags to be used in rendering
    ///   source text.
    fn layout_references<A: Clone>(
        custom_colors: &BTreeMap<i32, A>,
        references: &BTreeMap<i32, flow_parser::loc::Loc>,
    ) -> (BTreeMap<i32, Color<A>>, Tags) {
        use flow_parser::loc::Position;

        type TagList = Vec<(Position, i32, TagKind)>;

        fn add_tags<A: Clone>(
            custom_colors: &BTreeMap<i32, A>,
            mut colors: BTreeMap<i32, Color<A>>,
            mut opened: Opened,
            id: i32,
            start: &Position,
            end_pos: &Position,
            tags: TagList,
            mut tags_acc: TagList,
        ) -> (BTreeMap<i32, Color<A>>, TagList) {
            let mut tags_iter = tags.into_iter();
            while let Some((pos, tag_id, tag_kind)) = tags_iter.next() {
                let k = pos.cmp(start);

                if k == std::cmp::Ordering::Less
                    || (k == std::cmp::Ordering::Equal
                        && match tag_kind {
                            TagKind::Close => false,
                            TagKind::Open(tag_end) => end_pos < &tag_end,
                        })
                {
                    // Keep track of the ids which are opening and closing so that we can
                    // increment their colors if start is inside them.
                    //
                    // Also keep track of the ids which were opened at the time this tag was
                    // opened. This allows us to move backwards through the containment tree
                    // and update tag colors appropriately.
                    //
                    // See the definition of update_colors for more information.
                    opened = match tag_kind {
                        TagKind::Open(_) => {
                            let mut new_map = opened.0.clone();
                            new_map.insert(tag_id, opened.clone());
                            Opened(new_map)
                        }
                        TagKind::Close => {
                            let mut new_map = opened.0.clone();
                            new_map.remove(&tag_id);
                            Opened(new_map)
                        }
                    };

                    tags_acc.push((pos, tag_id, tag_kind));
                    continue;
                }

                // We've found the correct place for start! If there are any tags which
                // are currently open then we need to increment their colors.
                // Add our closing tag. We will get from this operation the color which
                // we should add for our current reference.
                let (color, tags) = add_close_tag(
                    &colors,
                    id,
                    end_pos,
                    std::iter::once((pos, tag_id, tag_kind)).chain(tags_iter),
                    0,
                    Vec::new(),
                );

                // Add a color for this id
                colors = match custom_colors.get(&id) {
                    Some(custom) => {
                        colors.insert(id, Color::CustomColor(custom.clone()));
                        colors
                    }
                    None => {
                        // Increment the colors of all open references by the color of this tag
                        //  It is a logic error if some open_id does not exist in colors.
                        colors = update_colors(&opened, colors, color);
                        // Add the color for this reference
                        colors.insert(id, Color::Color(color));
                        colors
                    }
                };

                // Finish by adding an open tag. A corresponding closing tag will have
                // been added by add_close_tag
                tags_acc.push((*start, id, TagKind::Open(*end_pos)));
                tags_acc.extend(tags);
                return (colors, tags_acc);
            }

            // For an empty array, add both the open and close tags.
            // Also add a color of 0 for this id.
            let color = match custom_colors.get(&id) {
                Some(custom) => Color::CustomColor(custom.clone()),
                None => Color::Color(0),
            };
            colors.insert(id, color);
            tags_acc.push((*start, id, TagKind::Open(*end_pos)));
            tags_acc.push((*end_pos, id, TagKind::Close));
            (colors, tags_acc)
        }

        fn add_close_tag<A: Clone>(
            colors: &BTreeMap<i32, Color<A>>,
            id: i32,
            end_pos: &Position,
            tags: impl IntoIterator<Item = (Position, i32, TagKind)>,
            mut color_acc: i32,
            mut tags_acc: TagList,
        ) -> (i32, TagList) {
            let mut tags_iter = tags.into_iter();

            while let Some((pos, tag_id, tag_kind)) = tags_iter.next() {
                if &pos <= end_pos {
                    // If we run into an open tag then our color must be at least 1 greater
                    // than the color of the opened tag
                    color_acc = match &tag_kind {
                        TagKind::Close => color_acc,
                        TagKind::Open(_) => match colors.get(&tag_id) {
                            None => std::cmp::max(color_acc, 1),
                            Some(Color::Color(tag_color)) => {
                                std::cmp::max(color_acc, tag_color + 1)
                            }
                            Some(Color::CustomColor(_)) => color_acc,
                        },
                    };

                    tags_acc.push((pos, tag_id, tag_kind));
                    continue;
                }

                // When we find the location for our close tag, add it
                tags_acc.push((*end_pos, id, TagKind::Close));
                tags_acc.push((pos, tag_id, tag_kind));
                tags_acc.extend(tags_iter);
                return (color_acc, tags_acc);
            }
            tags_acc.push((*end_pos, id, TagKind::Close));
            (color_acc, tags_acc)
        }

        fn update_colors<A: Clone>(
            opened: &Opened,
            mut colors: BTreeMap<i32, Color<A>>,
            color: i32,
        ) -> BTreeMap<i32, Color<A>> {
            for (open_id, nested_opened) in &opened.0 {
                let open_color = colors.get(open_id).cloned().unwrap_or(Color::Color(0));

                match open_color {
                    Color::CustomColor(_) => {}
                    Color::Color(open_color_val) => {
                        if open_color_val < color + 1 {
                            colors.insert(*open_id, Color::Color(color + 1));
                            colors = update_colors(nested_opened, colors, color + 1);
                        }
                    }
                }
            }
            colors
        }

        let mut colors: BTreeMap<i32, Color<A>> = BTreeMap::new();
        let mut file_tags: Tags = Tags::default();

        for (id, loc) in references {
            if let Some(source) = &loc.source {
                let source_key = source.clone();
                let tags = file_tags.0.get(&source_key).cloned().unwrap_or_default();
                let (new_colors, new_tags) = add_tags(
                    custom_colors,
                    colors,
                    Opened::default(),
                    *id,
                    &loc.start,
                    &loc.end,
                    tags,
                    Vec::new(),
                );
                colors = new_colors;
                file_tags.0.insert(source_key, new_tags);
            }
        }

        (colors, file_tags)
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(super) enum CustomColorKind {
        Primary,
        Root,
    }

    /// To be used with the result of layout_references.
    ///
    /// There are some special values influenced by logic
    /// in layout_friendly_error_group.
    ///
    /// - We manually set the color of primary references to CustomColor `Primary
    ///   which should always be red.
    /// - We manually set the color of root locations to CustomColor `Root which
    ///   should always be the default terminal color.
    fn get_tty_color_internal(color: &Color<CustomColorKind>) -> tty::RawColor {
        match color {
            Color::CustomColor(CustomColorKind::Primary) => tty::RawColor::Red,
            Color::CustomColor(CustomColorKind::Root) => tty::RawColor::Default,
            Color::Color(rank) => match rank % 4 {
                0 => tty::RawColor::Cyan,
                1 => tty::RawColor::Yellow,
                2 => tty::RawColor::Green,
                3 => tty::RawColor::Magenta,
                _ => panic!("unreachable"),
            },
        }
    }

    fn get_tty_color(id: i32, colors: &BTreeMap<i32, Color<CustomColorKind>>) -> tty::RawColor {
        let color = colors.get(&id).cloned().unwrap_or(Color::Color(0));
        get_tty_color_internal(&color)
    }

    /// Gets the Tty color from a stack of ids.
    /// This function will ignore CustomColor `Root if there are other colors on the stack.
    fn get_tty_color_from_stack(
        ids: &[i32],
        colors: &BTreeMap<i32, Color<CustomColorKind>>,
    ) -> Option<tty::RawColor> {
        if ids.is_empty() {
            return None;
        }

        let id = ids[0];
        let color = colors.get(&id).cloned().unwrap_or(Color::Color(0));

        match color {
            Color::CustomColor(CustomColorKind::Root) => {
                match get_tty_color_from_stack(&ids[1..], colors) {
                    Some(c) => Some(c),
                    None => Some(tty::RawColor::Default),
                }
            }
            color => Some(get_tty_color_internal(&color)),
        }
    }

    /// ==================
    /// Error Message Text
    /// ==================
    ///
    /// Text of an error message decorated with styles. We also accumulate all
    /// the references in the message which will be rendered in another step.
    ///
    /// Most of the following code is responsible for splitting our message onto
    /// multiple lines.
    pub(super) fn print_message_friendly(
        flags: &ErrorFlags,
        colors: &BTreeMap<i32, Color<CustomColorKind>>,
        indentation: Option<(friendly::BulletPointKind, usize)>,
        message: friendly::Message<i32>,
    ) -> Vec<(tty::Style, String)> {
        type Word = (usize, Vec<(tty::Style, String)>);

        enum Breakpoint {
            NonBreakpoint,
            Space(usize),
            NewLine(usize),
        }

        /// Split styled strings into words
        /// Takes input of (breakable, style, string) and returns list of words
        fn split_into_words(strs: Vec<(bool, tty::Style, String)>) -> Vec<Word> {
            fn merge(style: tty::Style, s: &str, word_in_progress: Option<Word>) -> Word {
                match word_in_progress {
                    // If we do not have a word in progress then create a new word from str.
                    None => (s.len(), vec![(style, s.to_string())]),
                    // If there is another word then we want to merge str with
                    // that word. We can assume that there is no breakpoint between
                    // the end of str and the beginning of the next word.
                    Some((len, mut word)) => {
                        word.push((style, s.to_string()));
                        (s.len() + len, word)
                    }
                }
            }

            let mut finished_words = Vec::new();
            let mut word_in_progress = None;
            let mut strs = VecDeque::from(strs);
            while let Some((breakable, style, s)) = strs.pop_front() {
                let bp = if breakable {
                    match (s.find(' '), s.find('\n')) {
                        (Some(i), Some(j)) if i < j => Breakpoint::Space(i),
                        (Some(i), None) => Breakpoint::Space(i),
                        (_, Some(j)) => Breakpoint::NewLine(j),
                        (None, None) => Breakpoint::NonBreakpoint,
                    }
                } else {
                    Breakpoint::NonBreakpoint
                };

                match bp {
                    Breakpoint::NonBreakpoint => {
                        let new_word = Some(merge(style, &s, word_in_progress));
                        word_in_progress = new_word;
                    }
                    Breakpoint::Space(bp) => {
                        let left = &s[..bp];
                        let right = &s[bp + 1..];
                        finished_words.push(merge(style, left, word_in_progress));
                        strs.push_front((true, style, right.to_string()));
                        word_in_progress = None;
                    }
                    Breakpoint::NewLine(bp) => {
                        let left = &s[..bp];
                        let right = &s[bp + 1..];
                        finished_words.push(merge(style, left, word_in_progress));
                        finished_words.push((0, vec![(style, "\n".to_string())]));
                        strs.push_front((true, style, right.to_string()));
                        word_in_progress = None;
                    }
                }
            }
            if let Some((n, words)) = word_in_progress {
                finished_words.push((n, words));
            }
            finished_words
        }

        fn is_style_underlined(style: tty::Style) -> bool {
            matches!(
                style,
                tty::Style::Underline(_)
                    | tty::Style::BoldUnderline(_)
                    | tty::Style::DimUnderline(_)
            )
        }

        fn is_first_underlined(styles: &[(tty::Style, String)]) -> bool {
            styles.first().is_some_and(|(s, _)| is_style_underlined(*s))
        }

        fn is_last_underlined(styles: &[(tty::Style, String)]) -> bool {
            styles.last().is_some_and(|(s, _)| is_style_underlined(*s))
        }

        fn with_len(
            opt: Option<Vec<(tty::Style, String)>>,
        ) -> Option<(usize, Vec<(tty::Style, String)>)> {
            opt.map(|styles| {
                let len = styles.iter().map(|(_, s)| s.len()).sum();
                (len, styles)
            })
        }

        /// Hard breaks a single Tty style list returned by split_into_words. We use
        /// this when a word is, by itself, larger then our line length. Returns a:
        ///
        /// ```text
        /// (int * (Tty.style * string) list)
        /// ```
        ///
        /// Where int represents the length of the last line.
        fn hard_break_styles(
            styles: Vec<(tty::Style, String)>,
            line_length: usize,
        ) -> (usize, Vec<(tty::Style, String)>) {
            let mut acc: Vec<(tty::Style, String)> = Vec::new();
            let mut pos = 0;
            let mut remaining = VecDeque::from(styles);

            while let Some((style, s)) = remaining.pop_front() {
                let bp = line_length.saturating_sub(pos);
                let len = s.len();

                if len > bp {
                    let left = &s[..bp];
                    let right = &s[bp..];
                    acc.push((style, left.to_string()));
                    acc.push(default_style("\n"));
                    pos = 0;
                    remaining.push_front((style, right.to_string()));
                } else {
                    acc.push((style, s.to_string()));
                    pos += len;
                }
            }

            (pos, acc)
        }

        /// Concatenates a words data structure created by split_into_words into a:
        ///
        /// ```text
        /// (Tty.style * string) list
        /// ```
        ///
        /// Which can be rendered by the Tty module. This is where we will line
        /// break depending on the length of our word and the position of the word
        /// on the current line.
        ///
        /// TODO: Handle orphans gracefully.
        fn concat_words_into_lines(
            line_length: usize,
            indentation_first: Option<Vec<(tty::Style, String)>>,
            indentation: Option<Vec<(tty::Style, String)>>,
            words: Vec<Word>,
        ) -> Vec<(tty::Style, String)> {
            let indentation = with_len(indentation);
            let indentation_first = match indentation_first {
                Some(_) => with_len(indentation_first),
                None => indentation.clone(),
            };
            let mut words_iter = words.into_iter();
            let Some((init_len, init_word)) = words_iter.next() else {
                // No words means no string.
                return indentation_first.map_or(Vec::new(), |(_, s)| s);
            };
            // If we have a single word we will use that as our initializer for
            // our fold on the rest of our words.
            let init = {
                let is_init_last_underlined = is_last_underlined(&init_word);
                let init = match indentation_first {
                    None => (init_len, vec![init_word]),
                    Some((indentation_first_len, indentation_first)) => (
                        indentation_first_len + init_len,
                        vec![indentation_first, init_word],
                    ),
                };
                (is_init_last_underlined, true, init)
            };
            let (_, _, (_, acc)) = words_iter.fold(
                init,
                |(last_underlined, initial_space, (pos, mut acc)), (len, word)| {
                    // If our position on the line plus one (for the space we would
                    // insert) plus the length of our word will fit in our line length
                    // then add the word to the current line separated from acc with a
                    // space. Otherwise start a new line where word is the only text.
                    if matches!(word.as_slice(), [(_, s)] if s == "\n") {
                        let last_underlined = is_last_underlined(&word);
                        let (newline_len, newline) = match &indentation {
                            None => (0, vec![default_style("\n")]),
                            Some((indentation_len, indentation)) => {
                                let mut newline = vec![default_style("\n")];
                                newline.extend(indentation.iter().cloned());
                                (*indentation_len, newline)
                            }
                        };
                        acc.push(newline);
                        (last_underlined, false, (newline_len, acc))
                    } else if pos + 1 + len > line_length {
                        let last_underlined = is_last_underlined(&word);
                        let (newline_len, newline) = match &indentation {
                            None => (0, vec![default_style("\n")]),
                            Some((indentation_len, indentation)) => {
                                let mut newline = vec![default_style("\n")];
                                newline.extend(indentation.iter().cloned());
                                (*indentation_len, newline)
                            }
                        };
                        if len <= line_length {
                            acc.push(newline);
                            acc.push(word);
                            (last_underlined, true, (len + newline_len, acc))
                        } else {
                            let (len, word) = hard_break_styles(word, line_length);
                            acc.push(newline);
                            acc.push(word);
                            (last_underlined, true, (len + newline_len, acc))
                        }
                    } else {
                        // If both the end of the last word was underlined *and* the
                        // beginning of the next word is underlined then we also want to
                        // underline the space we insert.
                        let should_underline = is_first_underlined(&word) && last_underlined;
                        let last_underlined = is_last_underlined(&word);
                        if !initial_space {
                            acc.push(word);
                            (last_underlined, true, (pos + len, acc))
                        } else {
                            let space = {
                                let style = if should_underline {
                                    tty::Style::Underline(tty::RawColor::Default)
                                } else {
                                    tty::Style::Normal(tty::RawColor::Default)
                                };
                                (style, " ".to_string())
                            };
                            let mut word_with_space = vec![space];
                            word_with_space.extend(word);
                            acc.push(word_with_space);
                            (last_underlined, true, (pos + 1 + len, acc))
                        }
                    }
                },
            );
            acc.into_iter().flatten().collect()
        }

        /// Create the tuple structure we pass into split_into_words. Code is not
        /// breakable but Text is breakable.
        fn print_message_inline(
            flags: &ErrorFlags,
            reference: bool,
            inline: friendly::MessageInline,
        ) -> (bool, tty::Style, String) {
            match inline {
                friendly::MessageInline::Code(s) if !should_color(flags.rendering_mode) => (
                    false,
                    tty::Style::Normal(tty::RawColor::Default),
                    format!("`{}`", s),
                ),
                friendly::MessageInline::Text(s) if reference => {
                    (true, tty::Style::Underline(tty::RawColor::Default), s)
                }
                friendly::MessageInline::Code(s) if reference => {
                    (false, tty::Style::BoldUnderline(tty::RawColor::Default), s)
                }
                friendly::MessageInline::Text(s) => {
                    (true, tty::Style::Normal(tty::RawColor::Default), s)
                }
                friendly::MessageInline::Code(s) => {
                    (false, tty::Style::Bold(tty::RawColor::Default), s)
                }
            }
        }

        // Put it all together!

        let mut message_parts: Vec<(bool, tty::Style, String)> = Vec::new();

        for feature in message.0 {
            match feature {
                friendly::MessageFeature::Inline(inlines) => {
                    for inline in inlines {
                        message_parts.push(print_message_inline(flags, false, inline));
                    }
                }
                friendly::MessageFeature::Reference(inlines, id) => {
                    for inline in inlines {
                        message_parts.push(print_message_inline(flags, true, inline));
                    }
                    message_parts.push((
                        false,
                        tty::Style::Normal(tty::RawColor::Default),
                        " ".to_string(),
                    ));
                    message_parts.push((
                        false,
                        tty::Style::Dim(tty::RawColor::Default),
                        "[".to_string(),
                    ));
                    message_parts.push((
                        false,
                        tty::Style::Normal(get_tty_color(id, colors)),
                        id.to_string(),
                    ));
                    message_parts.push((
                        false,
                        tty::Style::Dim(tty::RawColor::Default),
                        "]".to_string(),
                    ));
                }
            }
        }

        let (indentation_first, indentation_rest) = match indentation {
            None => (None, None),
            Some((kind, indent)) => {
                // Create the indentation for our message. The first line of indentation
                // will contain a bullet character.
                let space = " ".repeat((indent.saturating_sub(1)) * 3);
                let bullet = friendly::bullet_char(flags.unicode, kind);
                let indentation_first = vec![default_style(&format!("{} {} ", space, bullet))];
                let indentation_rest = vec![default_style(&format!("{}   ", space))];
                (Some(indentation_first), Some(indentation_rest))
            }
        };
        let words = split_into_words(message_parts);
        let mut result = concat_words_into_lines(
            flags.message_width as usize,
            indentation_first,
            indentation_rest,
            words,
        );
        result.push(default_style("\n"));
        result
    }

    /// Shows 3 lines of context in both directions from the root location.
    const ROOT_CONTEXT_LINES: i32 = 3;

    /// Omit code that takes up more than 15 lines in either direction.
    /// For 30 lines in total.
    ///
    /// 30 lines is a lot of code. The reasoning behind this is we want to show as
    /// much context as possible.
    const OMIT_AFTER_LINES: i32 = 15;

    /// Merge locs within only 3 lines of one another.
    const MERGE_NEARBY_LINES: i32 = 3;

    /// Get the vertical line character.
    /// When Unicode characters are enabled, the gutter divider is a box
    /// drawing [U+2502] character. Otherwise we use an ascii pipe symbol.
    ///
    /// [1]: http://graphemica.com/%E2%94%82
    fn vertical_line(flags: &ErrorFlags) -> &'static str {
        if flags.unicode {
            "\u{2502}" // │
        } else {
            "|"
        }
    }

    fn print_file_key(strip_root: Option<&std::path::Path>, file_key: Option<&FileKey>) -> String {
        use flow_parser::file_key::FileKeyInner;

        match file_key {
            Some(fk) => match fk.inner() {
                FileKeyInner::LibFile(_) => relative_lib_path(strip_root, &fk.to_absolute()),
                FileKeyInner::SourceFile(_)
                | FileKeyInner::JsonFile(_)
                | FileKeyInner::ResourceFile(_) => relative_path(strip_root, &fk.to_absolute()),
            },
            None => "(builtins)".to_string(),
        }
    }

    // todo: exception Oh_no_file_contents_have_changed

    /// Error indicating file contents have changed since parsing
    #[derive(Debug)]
    struct OhNoFileContentsHaveChanged;

    /// ===================================================
    /// Error Message Code Frames with Coalesced References
    /// ===================================================
    ///
    /// We render the root location for our friendly error message. Decorated with
    /// the reference locations from the message. When references are close together
    /// they are coalesced into a single code block.
    fn print_colorful_code_frames_friendly_with_coalesced_references(
        stdin_file: &StdinFile,
        strip_root: Option<&std::path::Path>,
        flags: &ErrorFlags,
        references: &BTreeMap<i32, flow_parser::loc::Loc>,
        colors: &BTreeMap<i32, Color<CustomColorKind>>,
        tags: &Tags,
        root_loc: &flow_parser::loc::Loc,
    ) -> Vec<(tty::Style, String)> {
        use flow_parser::loc::Loc;
        use flow_parser::loc::Position;

        // Get a list of all the locations we will want to display. We want to
        // display references and the root location with some extra lines
        // for context.
        let locs = {
            // Expand the root location with 3 lines of context in either direction.
            // However, don't expand before the first line or after the last line. If
            // we expand past the last line then read_lines_in_file will skip
            // those lines.
            let expanded_root_loc = {
                let start_line = std::cmp::max(1, root_loc.start.line - ROOT_CONTEXT_LINES);
                let end_line = root_loc.end.line + ROOT_CONTEXT_LINES;
                Loc {
                    source: root_loc.source.dupe(),
                    start: Position {
                        line: start_line,
                        ..root_loc.start
                    },
                    end: Position {
                        line: end_line,
                        ..root_loc.end
                    },
                }
            };
            let mut locs = vec![expanded_root_loc];
            locs.extend(references.values().duped());
            locs
        };

        // Group our locs by their file key.
        //
        // Also split large locs into two smaller locs.
        //
        // Also compute the largest line number. We need this to compute the
        // gutter width.
        let (max_line, locs): (i32, BTreeMap<FileKey, Vec<Loc>>) =
            locs.into_iter()
                .fold((0, BTreeMap::new()), |(max_line, mut acc), loc| {
                    let Some(source) = loc.source.dupe() else {
                        panic!("expected loc to have a source");
                    };
                    // If our loc is larger then some threshold determined by
                    // omit_after_lines then split it into two locs.
                    let new_locs = if loc.end.line - loc.start.line < OMIT_AFTER_LINES * 2 {
                        vec![loc.dupe()]
                    } else {
                        let loc1 = Loc {
                            source: loc.source.dupe(),
                            start: loc.start,
                            end: Position {
                                line: loc.start.line + (OMIT_AFTER_LINES - 1),
                                ..loc.start
                            },
                        };
                        let loc2 = Loc {
                            source: loc.source.dupe(),
                            start: Position {
                                line: loc.end.line - (OMIT_AFTER_LINES - 1),
                                ..loc.end
                            },
                            end: loc.end,
                        };
                        vec![loc1, loc2]
                    };
                    // Add the new locs to our map.
                    let file_locs = acc.entry(source).or_default();
                    file_locs.extend(new_locs);
                    (std::cmp::max(max_line, loc.end.line), acc)
                });

        // Perform some organization operations on our locs.
        let locs: BTreeMap<FileKey, Vec<Loc>> = locs
            .into_iter()
            .map(|(file_key, mut locs)| {
                // Sort all of the locations we want to display. Locations in the root
                // file should appear first. Sort in the reverse direction. Our next merge
                // step will flip the list back around.
                locs.sort();
                // Merge the locations we want to display. We start with the location with
                // the lowest line number. Our fold depends on this to merge correctly.
                let locs =
                    locs.into_iter()
                        .fold(Vec::<Loc>::new(), |mut acc, loc| match acc.last() {
                            // Init.
                            None => {
                                acc.push(loc);
                                acc
                            }
                            // If the previous loc + 3 lines below intersects with the next loc then
                            // we want to merge those locs into one code frame.
                            Some(last_loc) => {
                                let extended_last_loc = Loc {
                                    source: last_loc.source.dupe(),
                                    start: last_loc.start,
                                    end: Position {
                                        line: last_loc.end.line + (MERGE_NEARBY_LINES + 1),
                                        ..last_loc.end
                                    },
                                };
                                if loc.lines_intersect(&extended_last_loc) {
                                    let last_loc = acc.pop().unwrap();
                                    let merged = Loc {
                                        source: last_loc.source.dupe(),
                                        start: if loc.start < last_loc.start {
                                            loc.start
                                        } else {
                                            last_loc.start
                                        },
                                        end: if loc.end > last_loc.end {
                                            loc.end
                                        } else {
                                            last_loc.end
                                        },
                                    };
                                    acc.push(merged);
                                    acc
                                } else {
                                    // Otherwise, add the loc by itself.
                                    acc.push(loc);
                                    acc
                                }
                            }
                        });
                // Return the reversed locs.
                (file_key, locs)
            })
            .collect();

        // Organize all our references onto the line which we will find them on. We
        // do a second pass to sort these references and determine the
        // gutter width.
        let file_line_references: BTreeMap<FileKey, BTreeMap<i32, Vec<(i32, Position)>>> =
            references
                .iter()
                .fold(BTreeMap::new(), |mut acc, (id, loc)| {
                    if *id < 0 {
                        return acc;
                    }
                    let Some(source) = loc.source.dupe() else {
                        panic!("expected loc to have a source");
                    };
                    let line_references = acc.entry(source).or_default();
                    let references_on_line: &mut Vec<(i32, Position)> =
                        line_references.entry(loc.start.line).or_default();
                    references_on_line.push((*id, loc.start));
                    acc
                });

        // Create the styled text which we will put in the gutter for all of our
        // file line references.
        //
        // We use a ref for gutter_width since we want to map file_line_references
        // in place instead of using a fold which would re-create the map.
        let mut gutter_width = 5usize;
        let file_line_references: BTreeMap<
            FileKey,
            BTreeMap<i32, (usize, Vec<(tty::Style, String)>)>,
        > = file_line_references
            .into_iter()
            .map(|(file_key, line_refs)| {
                let line_refs = line_refs
                    .into_iter()
                    .map(|(line, mut references)| {
                        // Reverse sort the references by their start position. We reverse sort
                        // since we fold_left next.
                        references.sort_by(|(_, a), (_, b)| b.cmp(a));
                        // Fold the list. Creating the width and the string we will
                        // ultimately render.
                        let (width, mut refs_styled): (usize, VecDeque<(tty::Style, String)>) =
                            references.iter().fold(
                                (1, VecDeque::from([default_style(" ")])),
                                |(width, mut acc), (id, _)| {
                                    let string_id = id.to_string();
                                    let width = width + 2 + string_id.len();
                                    acc.push_front(dim_style("]"));
                                    acc.push_front((
                                        tty::Style::Normal(get_tty_color(*id, colors)),
                                        string_id,
                                    ));
                                    acc.push_front(dim_style("["));
                                    (width, acc)
                                },
                            );
                        let width = width + 1;
                        refs_styled.push_front(default_style(" "));
                        let refs_styled: Vec<(tty::Style, String)> = refs_styled.into();
                        // Set gutter_width to the larger of the current gutter_width or the width
                        // for this line.
                        gutter_width = std::cmp::max(gutter_width, width);
                        // Return the final list of references for the line along with the width.
                        (line, (width, refs_styled))
                    })
                    .collect();
                (file_key, line_refs)
            })
            .collect();

        // Get the line number gutter length by looking at the string length for the
        // maximum line number.
        //
        // Sometimes, the maximum line number will not be read. So this might not be
        // the true maximum line number. However, for the purposes of
        // max_line_number_length this imprecision is not important.
        //
        // The penalty for this imprecision is our code frame gutter might be a
        // little wider then it needs to be in unlikely edge cases.
        let max_line_number_length = max_line.to_string().len();
        let vertical_line = vertical_line(flags);

        // Print the code frame for each loc. Highlighting appropriate references.
        let code_frames: BTreeMap<FileKey, Vec<(tty::Style, String)>> = locs
            .into_iter()
            .map(|(file_key, locs)| {
                // Used by read_lines_in_file.
                let filename = file_of_source(Some(&file_key));
                // Get some data structures associated with this file.
                let file_tags: Vec<(Position, i32, TagKind)> =
                    tags.0.get(&file_key).cloned().unwrap_or_default();
                let line_references = file_line_references
                    .get(&file_key)
                    .cloned()
                    .unwrap_or_default();

                // Fold all the locs for this file into code frames.
                let (_, _, code_frames): (
                    Vec<(Position, i32, TagKind)>,
                    Vec<i32>,
                    Vec<Vec<(tty::Style, String)>>,
                ) = locs.into_iter().fold(
                    (file_tags, Vec::new(), Vec::new()),
                    |(tags, opened, mut code_frames), loc| {
                        // Read the lines from this location.
                        let lines = read_lines_in_file(&loc, filename.as_deref(), stdin_file);
                        let Some((first_line, rest_lines)) = lines else {
                            // Failed to read the file, so skip this code frame
                            return (tags, opened, code_frames);
                        };
                        let lines = {
                            let mut lines = vec![first_line];
                            lines.extend(rest_lines);
                            lines
                        };

                        // Loop which will paint the different parts of a line of code in
                        // our code frame. Eats tags on the current line.
                        fn loop_fn(
                            mut acc: Vec<(tty::Style, String)>,
                            col: i32,
                            mut tags: Vec<(Position, i32, TagKind)>,
                            mut opened: Vec<i32>,
                            line: &str,
                            n: i32,
                            colors: &BTreeMap<i32, Color<CustomColorKind>>,
                        ) -> Result<
                            (
                                Vec<(Position, i32, TagKind)>,
                                Vec<i32>,
                                Vec<(tty::Style, String)>,
                            ),
                            OhNoFileContentsHaveChanged,
                        > {
                            // Get the current style for the line.
                            let style = match get_tty_color_from_stack(&opened, colors) {
                                Some(color) => tty::Style::Normal(color),
                                None => tty::Style::Dim(tty::RawColor::Default),
                            };
                            let tag_on_this_line = tags
                                .first()
                                .map(|(pos, _, _)| pos.line == n)
                                .unwrap_or(false);
                            if tags.is_empty() {
                                // If we have no more tags then use our current style with
                                // the line.
                                acc.push((style, line.to_string()));
                                Ok((tags, opened, acc))
                            } else if tag_on_this_line {
                                // If we have a tag on this line then eat it and add the new
                                // opened tag to `opened`. Note that our condition depends on tag
                                // being well formed by layout_references!
                                let (pos, tag_id, tag_kind) = tags.remove(0);
                                opened = match tag_kind {
                                    TagKind::Open(_) => {
                                        let mut new_opened = vec![tag_id];
                                        new_opened.extend(opened);
                                        new_opened
                                    }
                                    TagKind::Close => {
                                        opened.into_iter().filter(|id| *id != tag_id).collect()
                                    }
                                };
                                let split = (pos.column - col) as usize;
                                // TODO: Get a SHA for each file when we parse it, and include the SHA in the
                                // loc. Then, we can know for sure whether a file has changed or not when we
                                // go to pretty print an error.
                                //
                                // Here we only know for sure that a file has changed when a particular line
                                // is too short, which means we can sometimes print bad code frames.
                                let (left, right) = if split <= line.len() {
                                    (&line[..split], &line[split..])
                                } else {
                                    return Err(OhNoFileContentsHaveChanged);
                                };
                                acc.push((style, left.to_string()));
                                loop_fn(acc, pos.column, tags, opened, right, n, colors)
                            } else {
                                // If we do not have a tag on this line then use our current style
                                // with this line of code.
                                acc.push((style, line.to_string()));
                                Ok((tags, opened, acc))
                            }
                        }

                        // Create the code frame styles.
                        let tags_backup = tags.clone();
                        let opened_backup = opened.clone();
                        let result: Result<
                            (
                                i32,
                                Vec<(Position, i32, TagKind)>,
                                Vec<i32>,
                                Vec<Vec<(tty::Style, String)>>,
                            ),
                            OhNoFileContentsHaveChanged,
                        > = lines.into_iter().try_fold(
                            (loc.start.line, tags, opened, Vec::new()),
                            |(n, tags, opened, mut acc), line| {
                                // Start that loop!
                                let (tags, opened, code_line) =
                                    loop_fn(Vec::new(), 0, tags, opened, &line, n, colors)?;
                                // Create the gutter text.
                                let gutter: Vec<(tty::Style, String)> = match line_references
                                    .get(&n)
                                {
                                    None => {
                                        vec![default_style(&" ".repeat(gutter_width))]
                                    }
                                    Some((width, references)) if *width < gutter_width => {
                                        let mut g =
                                            vec![default_style(&" ".repeat(gutter_width - width))];
                                        g.extend(references.clone());
                                        g
                                    }
                                    Some((_, references)) => references.clone(),
                                };
                                // Create the next line.
                                let next_line: Vec<(tty::Style, String)> = {
                                    // Get the line number string with appropriate padding.
                                    let line_number = {
                                        let n_str = n.to_string();
                                        let padding =
                                            " ".repeat(max_line_number_length - n_str.len());
                                        vec![
                                            default_style(&padding),
                                            (tty::Style::Dim(tty::RawColor::Default), n_str),
                                            dim_style(vertical_line),
                                        ]
                                    };
                                    let mut next_line = gutter;
                                    next_line.extend(line_number);
                                    // If the line is empty then strip the whitespace which would be
                                    // trailing whitespace anyways.
                                    if !line.is_empty() {
                                        next_line.push(default_style(" "));
                                    }
                                    next_line.extend(code_line);
                                    next_line.push(default_style("\n"));
                                    next_line
                                };
                                // Increment our line count and add the next line to
                                // our accumulator.
                                acc.push(next_line);
                                Ok((n + 1, tags, opened, acc))
                            },
                        );

                        match result {
                            Ok((_, tags, opened, code_frame)) => {
                                code_frames.push(code_frame.into_iter().flatten().collect());
                                (tags, opened, code_frames)
                            }
                            Err(OhNoFileContentsHaveChanged) => {
                                // Realized the file has changed, so skip this code frame
                                (tags_backup, opened_backup, code_frames)
                            }
                        }
                    },
                );

                let code_frame: Vec<(tty::Style, String)> = match code_frames.split_first() {
                    None => Vec::new(),
                    Some((first_code_frame, rest_code_frames)) => {
                        // Add all of our code frames together with a colon for omitted chunks
                        // of code in the file.
                        rest_code_frames
                            .iter()
                            .fold(vec![first_code_frame.clone()], |mut acc, code_frame| {
                                acc.push(vec![
                                    default_style(
                                        &" ".repeat(gutter_width + max_line_number_length),
                                    ),
                                    dim_style(":"),
                                    default_style("\n"),
                                ]);
                                acc.push(code_frame.clone());
                                acc
                            })
                            .into_iter()
                            .flatten()
                            .collect()
                    }
                };

                (file_key, code_frame)
            })
            .collect();

        // Get the root code frame from our map of code frames. We will start with
        // this code frame.
        let root_file_key = root_loc
            .source
            .dupe()
            .expect("expected loc to have a source");
        let root_code_frame = code_frames.get(&root_file_key).cloned();
        let code_frames: BTreeMap<FileKey, Vec<(tty::Style, String)>> = code_frames
            .into_iter()
            .filter(|(k, _)| k != &root_file_key)
            .collect();

        // If we only have a root code frame then only render that.
        if code_frames.is_empty() {
            return root_code_frame.unwrap_or_default();
        }

        let mut code_frames: VecDeque<(FileKey, Vec<(tty::Style, String)>)> =
            code_frames.into_iter().collect();
        if let Some(root_code_frame) = root_code_frame {
            code_frames.push_front((root_file_key, root_code_frame));
        }

        // Add a title to non-root code frames and concatenate them all together!
        code_frames.into_iter().enumerate().fold(
            Vec::new(),
            |mut acc, (idx, (file_key, code_frame))| {
                if idx != 0 {
                    acc.push(default_style("\n"));
                }
                let file_key_str = print_file_key(strip_root, Some(&file_key));
                let header = vec![
                    default_style(&" ".repeat(gutter_width)),
                    default_style(&format!("{}\n", file_key_str)),
                ];
                acc.extend(header);
                acc.extend(code_frame);
                acc
            },
        )
    }

    /// ======================================================
    /// Error Message Code Frames without Coalesced References
    /// ======================================================
    ///
    /// Renders the root location along with reference locations, but
    /// without coalescing close references.
    fn print_code_frames_friendly_without_coalesced_references(
        stdin_file: &StdinFile,
        strip_root: Option<&std::path::Path>,
        flags: &ErrorFlags,
        references: &BTreeMap<i32, flow_parser::loc::Loc>,
        colors: &BTreeMap<i32, Color<CustomColorKind>>,
        root_reference_id: Option<i32>,
        root_loc: &flow_parser::loc::Loc,
    ) -> Vec<(tty::Style, String)> {
        use flow_parser::loc::Loc;

        let vertical_line = vertical_line(flags);
        let should_color = matches!(flags.rendering_mode, RenderingMode::IdeDetailedError);
        let dim_style_fn = |text: &str| -> (tty::Style, String) {
            if should_color {
                dim_style(text)
            } else {
                default_style(text)
            }
        };
        let styled_id = |id: i32| -> (tty::Style, String) {
            if should_color {
                (
                    tty::Style::Normal(get_tty_color(id, colors)),
                    format!(" [{}]", id),
                )
            } else {
                default_style(&format!(" [{}]", id))
            }
        };
        // Get the maximum end line number. We will use this for computing our
        // gutter width.
        let max_end_line = references
            .values()
            .map(|loc| loc.end.line)
            .fold(root_loc.end.line, std::cmp::max);
        // Get the max gutter extension length which is the length of the longest
        // line number plus 3.
        let gutter_width = 3 + max_end_line.to_string().len();

        // Prints a single, colorless, location.
        let print_loc = |with_filename: bool,
                         id: Option<i32>,
                         loc: &Loc|
         -> Option<Vec<(tty::Style, String)>> {
            // Get the lines for the location...
            let filename = file_of_source(loc.source.as_ref());
            let source_code_lines = filename
                .as_deref()
                .and_then(|f| read_lines_in_file(loc, Some(f), stdin_file));
            let colored = |text: &str| -> (tty::Style, String) {
                match id {
                    Some(id) if should_color => (
                        tty::Style::Normal(get_tty_color(id, colors)),
                        text.to_string(),
                    ),
                    _ => default_style(text),
                }
            };
            let styled_lines: Option<Vec<(tty::Style, String)>> =
                source_code_lines.map(|(first_line, rest_lines)| {
                    let mut line_list = vec![first_line];
                    line_list.extend(rest_lines);

                    // Print every line by appending the line number and appropriate
                    // gutter width.
                    let mut styled_code_block_lines: Vec<(tty::Style, String)> = Vec::new();
                    let mut n = loc.start.line;

                    for line in &line_list {
                        // If we show more lines then some upper limit omit any extra code.
                        if n >= loc.start.line + OMIT_AFTER_LINES
                            && n <= loc.end.line - OMIT_AFTER_LINES
                        {
                            if n == loc.start.line + OMIT_AFTER_LINES {
                                let gutter = " ".repeat(gutter_width);
                                styled_code_block_lines.push(default_style(&gutter));
                                styled_code_block_lines.push(default_style(":\n"));
                            }
                            // Otherwise skip this line
                        } else {
                            // Otherwise, render the line.
                            let n_string = n.to_string();
                            let gutter_space = " ".repeat(gutter_width - n_string.len());

                            styled_code_block_lines.push(default_style(&gutter_space));
                            styled_code_block_lines.push(dim_style_fn(&n_string));
                            styled_code_block_lines.push(dim_style_fn(vertical_line));

                            if !line.is_empty() {
                                styled_code_block_lines.push(default_style(" "));

                                // Try to color the line appropriately
                                let line_styled: Result<Vec<(tty::Style, String)>, ()> =
                                    (|| -> Result<Vec<(tty::Style, String)>, ()> {
                                        if !should_color {
                                            Ok(vec![default_style(line)])
                                        } else if n < loc.start.line || n > loc.end.line {
                                            // If the line is outside of the reference range,
                                            // color with default dim style
                                            Ok(vec![dim_style_fn(line)])
                                        } else if n > loc.start.line && n < loc.end.line {
                                            // If the line is in between start and end lines,
                                            // the entire line is colored.
                                            Ok(vec![colored(line)])
                                        } else if n == loc.start.line && n != loc.end.line {
                                            //     vvv
                                            // abcdefg <- we are here
                                            // hijklmn
                                            // ^^^
                                            let start_col = loc.start.column as usize;
                                            if start_col > line.len() {
                                                return Err(());
                                            }
                                            let first = &line[..start_col];
                                            let mid = &line[start_col..];
                                            Ok(vec![dim_style_fn(first), colored(mid)])
                                        } else if n == loc.end.line && n != loc.start.line {
                                            //     vvv
                                            // abcdefg
                                            // hijklmn <- we are here
                                            // ^^^
                                            let end_col = loc.end.column as usize;
                                            if end_col > line.len() {
                                                return Err(());
                                            }
                                            let mid = &line[..end_col];
                                            let last = &line[end_col..];
                                            Ok(vec![colored(mid), dim_style_fn(last)])
                                        } else {
                                            // Single line reference
                                            let start_col = loc.start.column as usize;
                                            let end_col = loc.end.column as usize;
                                            if start_col > line.len() || end_col > line.len() {
                                                return Err(());
                                            }
                                            let first = &line[..start_col];
                                            let mid = &line[start_col..end_col];
                                            let last = &line[end_col..];
                                            Ok(vec![
                                                dim_style_fn(first),
                                                colored(mid),
                                                dim_style_fn(last),
                                            ])
                                        }
                                    })();

                                match line_styled {
                                    Ok(styles) => styled_code_block_lines.extend(styles),
                                    // If the substring failed, we might be in a state where the file content
                                    // has changed. Fall back to the non-coloring mode instead of crashing.
                                    Err(()) => {
                                        styled_code_block_lines.push(dim_style_fn(line));
                                    }
                                }
                            }

                            styled_code_block_lines.push(default_style("\n"));
                        }
                        n += 1;
                    }
                    // Get our gutter space for the underline and overline.
                    let gutter_space = " ".repeat(gutter_width + 2);
                    // Add the overline for our loc (for multi-line locations).
                    let mut styled_lines: Vec<(tty::Style, String)> = Vec::new();
                    if loc.start.line != loc.end.line {
                        let first_line_len = line_list.first().map(|s| s.len()).unwrap_or(0);
                        // In some cases, we create a location that starts at or after the
                        // end of a line. This probably shouldn't happen, but if it does, we
                        // can still create an overline with a carat pointing to that column
                        // position.
                        let first_line_len =
                            std::cmp::max(first_line_len, loc.start.column as usize + 1);
                        let start_col = loc.start.column as usize;
                        let overline = format!(
                            "{}v{}",
                            " ".repeat(start_col),
                            "-".repeat(first_line_len - start_col - 1)
                        );
                        styled_lines.push(default_style(&gutter_space));
                        styled_lines.push(colored(&overline));
                        styled_lines.push(default_style("\n"));
                    }

                    styled_lines.extend(styled_code_block_lines);

                    // Add the underline for our loc.
                    let styled_underline = if loc.start.line == loc.end.line {
                        let start_col = loc.start.column as usize;
                        let end_col = loc.end.column as usize;
                        colored(&format!(
                            "{}{}",
                            " ".repeat(start_col),
                            "^".repeat(end_col.saturating_sub(start_col))
                        ))
                    } else {
                        let empty_string = String::new();
                        let last_line = line_list.last().unwrap_or(&empty_string);
                        // Don't underline the whitespace at the beginning of the last line
                        let underline_prefix: String = last_line
                            .chars()
                            .take_while(|c| *c == ' ' || *c == '\t')
                            .collect();
                        let dash_length = (loc.end.column as usize)
                            .saturating_sub(underline_prefix.len())
                            .saturating_sub(1);
                        colored(&format!(
                            "{}{}{}",
                            underline_prefix,
                            "-".repeat(dash_length),
                            "^"
                        ))
                    };
                    styled_lines.push(default_style(&gutter_space));
                    styled_lines.push(styled_underline);
                    // If we have a reference id then add it just after the underline.
                    if let Some(id) = id {
                        if id > 0 {
                            styled_lines.push(styled_id(id));
                        }
                    }
                    // Add a final newline to lines.
                    styled_lines.push(default_style("\n"));
                    // Return our final lines string
                    styled_lines
                });

            // If we were configured to print the filename then add it to our lines
            // before returning.
            if !with_filename {
                styled_lines
            } else {
                let space = "   ";
                let filename_str = print_file_key(strip_root, loc.source.as_ref());
                match styled_lines {
                    None => {
                        // if the file has no lines -- couldn't read it, empty file, or loc
                        // is pointing at the whole file (line 0, col 0) -- then point at
                        // the filename itself.
                        let underline = "^".repeat(filename_str.len());
                        let styled_id_str = match id {
                            Some(id) if id > 0 => styled_id(id),
                            _ => default_style(""),
                        };
                        Some(vec![
                            default_style(space),
                            default_style(&filename_str),
                            default_style("\n"),
                            default_style(space),
                            default_style(&underline),
                            styled_id_str,
                            default_style("\n"),
                        ])
                    }
                    Some(styled_lines) => {
                        let filename_with_pos = format!(
                            "{}:{}:{}",
                            filename_str,
                            loc.start.line,
                            loc.start.column + 1
                        );
                        let mut result = vec![
                            default_style(space),
                            default_style(&filename_with_pos),
                            default_style("\n"),
                        ];
                        result.extend(styled_lines);
                        Some(result)
                    }
                }
            }
        };

        // Print the locations for all of our references.
        let mut reference_frames: Vec<Vec<(tty::Style, String)>> = Vec::new();
        for (id, loc) in references.iter() {
            let is_root = root_reference_id
                .map(|root_id| root_id == *id)
                .unwrap_or(false);
            // Skip this reference if either it is a "shadow reference" or it is the
            // reference for the root.
            if *id <= 0 || is_root {
                continue;
            }
            if let Some(styled_lines) = print_loc(true, Some(*id), loc) {
                reference_frames.push(styled_lines);
            }
        }
        let references_output: Vec<(tty::Style, String)> =
            reference_frames.into_iter().flatten().collect();
        // Add the "References:" label if we have some references.
        let references_output = if references_output.is_empty() {
            Vec::new()
        } else {
            let mut result = vec![default_style("\nReferences:\n")];
            result.extend(references_output);
            result
        };
        // Print the root location.
        let has_references = !references_output.is_empty();
        match print_loc(has_references, root_reference_id, root_loc) {
            Some(root_code) => {
                let mut result = root_code;
                result.extend(references_output);
                result
            }
            None => references_output,
        }
    }

    /// Goes through the process of laying out a friendly error message group by
    /// combining our lower level functions like extract_references_intermediate
    /// and layout_references.
    ///
    /// After we extract all of the reference locations from our message and give
    /// them an id we then add a couple of "shadow references" with negative ids to
    /// the references map. These shadow references include:
    ///
    /// - The root location of an error message.
    /// - The primary locations of all error messages in the group.
    ///
    /// These shadow references are not shown in the gutter since they are not
    /// referred to in the message text. Which is why they have a negative id.
    ///
    /// We add these "shadow references" into our reference system so that they may
    /// be laid out along with all the other references by layout_references.
    ///
    /// We generally also provide a custom color to layout_references for these
    /// "shadow references" so we don't use their default layout color.
    fn layout_friendly_error_group(
        root_loc: &flow_parser::loc::Loc,
        primary_locs: &std::collections::BTreeSet<flow_parser::loc::Loc>,
        message_group: friendly::MessageGroup<flow_parser::loc::Loc>,
    ) -> (
        BTreeMap<i32, flow_parser::loc::Loc>,
        Option<i32>,
        BTreeMap<i32, Color<CustomColorKind>>,
        Tags,
        friendly::MessageGroup<i32>,
    ) {
        use flow_parser::loc::Loc;

        // Setup our initial loc_to_id and id_to_loc maps.
        let (next_id, loc_to_id, id_to_loc) = (
            1i32,
            BTreeMap::<Loc, i32>::new(),
            BTreeMap::<i32, Loc>::new(),
        );

        // Extract all our references from the message group.
        let (next_id, loc_to_id, id_to_loc, message_group) =
            friendly::extract_references_intermediate(next_id, loc_to_id, id_to_loc, message_group);

        // Find all the references for primary locations. If there is not yet a
        // reference for a primary location then we create one.
        let (next_id, loc_to_id, id_to_loc, primary_loc_ids) = primary_locs.iter().fold(
            (
                next_id,
                loc_to_id,
                id_to_loc,
                std::collections::BTreeSet::<i32>::new(),
            ),
            |(mut next_id, mut loc_to_id, mut id_to_loc, mut primary_loc_ids), loc| {
                match loc_to_id.get(loc) {
                    // If there is a reference for this primary location then don't alter
                    // our loc_to_id or id_to_loc maps.
                    Some(&id) => {
                        primary_loc_ids.insert(id);
                    }
                    // If there is no reference for this primary location then create a
                    // negative id. Negative ids will not be rendered in the code
                    // frame gutter.
                    None => {
                        let id = -next_id;
                        next_id += 1;
                        loc_to_id.insert(loc.clone(), id);
                        id_to_loc.insert(id, loc.clone());
                        primary_loc_ids.insert(id);
                    }
                }
                (next_id, loc_to_id, id_to_loc, primary_loc_ids)
            },
        );
        // Go through a very similar process as primary locations to add a reference
        // for the root location and record its id. If a reference already exists
        // then we will not highlight our root location any differently!
        let (next_id, loc_to_id, id_to_loc, root_id, custom_root_color) =
            match loc_to_id.get(root_loc) {
                Some(&id) => (next_id, loc_to_id, id_to_loc, Some(id), false),
                None => {
                    let id = -next_id;
                    let next_id = next_id + 1;
                    let mut loc_to_id = loc_to_id;
                    let mut id_to_loc = id_to_loc;
                    loc_to_id.insert(root_loc.clone(), id);
                    id_to_loc.insert(id, root_loc.clone());
                    (next_id, loc_to_id, id_to_loc, Some(id), true)
                }
            };
        // Create a custom color map for our primary location and root locations.
        let mut custom_colors: BTreeMap<i32, CustomColorKind> = BTreeMap::new();
        // Set the custom color for all primary loc ids to `Primary.
        for &id in &primary_loc_ids {
            custom_colors.insert(id, CustomColorKind::Primary);
        }
        // Manually set the custom color for the root loc to `Root.
        if let Some(id) = root_id {
            if custom_root_color {
                custom_colors.insert(id, CustomColorKind::Root);
            }
        }
        // Layout all of our references. Including the negative ones.
        let (colors, tags) = layout_references(&custom_colors, &id_to_loc);
        // Return everything we need for printing error messages.
        let _ = (next_id, loc_to_id);
        (id_to_loc, root_id, colors, tags, message_group)
    }

    fn get_pretty_printed_friendly_error_group(
        stdin_file: &StdinFile,
        strip_root: Option<&std::path::Path>,
        flags: &ErrorFlags,
        severity: Severity,
        primary_locs: &std::collections::BTreeSet<flow_parser::loc::Loc>,
        message_group: friendly::MessageGroup<flow_parser::loc::Loc>,
    ) -> Vec<(tty::Style, String)> {
        // Get the primary location.
        let primary_loc = match primary_locs.iter().next() {
            Some(loc) => loc.clone(),
            None => return Vec::new(),
        };

        // The header location is the primary location
        let header = print_header_friendly(strip_root, flags, severity, &primary_loc);
        let root_loc = primary_loc;
        // Layout our entire friendly error group. This returns a bunch of data we
        // will need to print our friendly error group.
        let (references, root_reference_id, colors, tags, message_group) =
            layout_friendly_error_group(&root_loc, primary_locs, message_group);
        // Print the text of our error message by traversing the message_group.
        let message = {
            fn loop_message(
                flags: &ErrorFlags,
                colors: &BTreeMap<i32, Color<CustomColorKind>>,
                indentation: Option<(friendly::BulletPointKind, usize)>,
                acc: &mut Vec<Vec<(tty::Style, String)>>,
                message_group: friendly::MessageGroup<i32>,
            ) {
                acc.push(print_message_friendly(
                    flags,
                    colors,
                    indentation.clone(),
                    message_group.group_message,
                ));

                match message_group.group_message_nested {
                    friendly::GroupMessageNested::NoNesting => {}
                    friendly::GroupMessageNested::StackedErrorNesting(l) => {
                        let new_indentation = Some((
                            friendly::BulletPointKind::StackedErrorBulletPoint,
                            indentation.as_ref().map(|(_, i)| *i).unwrap_or(0) + 1,
                        ));
                        for mg in l {
                            loop_message(flags, colors, new_indentation.clone(), acc, mg);
                        }
                    }
                    friendly::GroupMessageNested::SpeculationErrorNesting(l) => {
                        let new_indentation = Some((
                            friendly::BulletPointKind::SpeculationErrorBulletPoint,
                            indentation.as_ref().map(|(_, i)| *i).unwrap_or(0) + 1,
                        ));
                        for mg in l {
                            loop_message(flags, colors, new_indentation.clone(), acc, mg);
                        }
                    }
                }

                if let Some(post) = message_group.group_message_post {
                    let post_indentation = indentation
                        .as_ref()
                        .map(|(_, i)| (friendly::BulletPointKind::StackedErrorBulletPoint, i + 1));
                    acc.push(print_message_friendly(
                        flags,
                        colors,
                        post_indentation,
                        post,
                    ));
                }
            }

            let mut acc = Vec::new();
            loop_message(flags, &colors, None, &mut acc, message_group);
            acc.into_iter().flatten().collect::<Vec<_>>()
        };

        // Print the code frame for our error message.
        let code_frame = match flags.rendering_mode {
            RenderingMode::CliColorAlways => {
                print_colorful_code_frames_friendly_with_coalesced_references(
                    stdin_file,
                    strip_root,
                    flags,
                    &references,
                    &colors,
                    &tags,
                    &root_loc,
                )
            }
            RenderingMode::CliColorAuto if tty::supports_color() => {
                print_colorful_code_frames_friendly_with_coalesced_references(
                    stdin_file,
                    strip_root,
                    flags,
                    &references,
                    &colors,
                    &tags,
                    &root_loc,
                )
            }
            RenderingMode::CliColorAuto
            | RenderingMode::CliColorNever
            | RenderingMode::IdeDetailedError => {
                print_code_frames_friendly_without_coalesced_references(
                    stdin_file,
                    strip_root,
                    flags,
                    &references,
                    &colors,
                    root_reference_id,
                    &root_loc,
                )
            }
        };

        // Put it all together!
        let mut result = Vec::new();
        // Header:
        result.extend(header);
        result.push(default_style("\n"));
        // Error Message:
        result.extend(message);
        // Code frame:
        if !code_frame.is_empty() {
            result.push(default_style("\n"));
            result.extend(code_frame);
        }
        // Next error:
        result.push(default_style("\n"));

        result
    }

    fn get_pretty_printed_error(
        stdin_file: &StdinFile,
        strip_root: Option<&std::path::Path>,
        flags: &ErrorFlags,
        severity: Severity,
        show_all_branches: bool,
        err: PrintableError<flow_parser::loc::Loc>,
    ) -> (bool, Vec<(tty::Style, String)>) {
        use flow_parser::loc::Loc;
        let PrintableError(error_kind, error) = err;
        // Singleton errors concatenate the optional error root with the error
        // message and render a single message.
        let (hidden_branches, primary_loc, message_group) = friendly::message_group_of_error(
            true,
            true,
            show_all_branches,
            flags.unicode,
            error_kind,
            error,
        );
        let had_hidden_branches = hidden_branches.is_some();
        let mut primary_locs = std::collections::BTreeSet::<Loc>::new();
        primary_locs.insert(primary_loc);
        let message_group = friendly::MessageGroup {
            group_message: friendly::capitalize(message_group.group_message),
            group_message_nested: message_group.group_message_nested,
            group_message_post: message_group.group_message_post,
        };
        let result = get_pretty_printed_friendly_error_group(
            stdin_file,
            strip_root,
            flags,
            severity,
            &primary_locs,
            message_group,
        );
        (had_hidden_branches, result)
    }

    fn print_styles<W: std::io::Write>(
        out: &mut W,
        flags: &ErrorFlags,
        styles: Vec<(tty::Style, String)>,
    ) -> std::io::Result<()> {
        let styles: Vec<(tty::Style, String)> = if flags.one_line {
            styles.iter().map(remove_newlines).collect()
        } else {
            styles
        };
        let color_mode = color_mode_of_rendering_mode(flags.rendering_mode);
        tty::cprint(out, &styles, Some(color_mode))?;
        tty::cprint(out, &[default_style("\n")], Some(color_mode))
    }

    pub fn format_single_styled_error_for_vscode(
        strip_root: Option<&std::path::Path>,
        severity: Severity,
        unsaved_content: &StdinFile,
        error: PrintableError<flow_parser::loc::Loc>,
    ) -> Vec<(tty::Style, String)> {
        let flags = ErrorFlags {
            rendering_mode: RenderingMode::IdeDetailedError,
            include_warnings: true,
            max_warnings: None,
            one_line: false,
            list_files: false,
            show_all_errors: true,
            show_all_branches: true,
            unicode: true,
            message_width: 80,
        };
        let (_, styles) =
            get_pretty_printed_error(unsaved_content, strip_root, &flags, severity, true, error);
        styles
    }

    pub fn format_errors<W: std::io::Write>(
        out: &mut W,
        flags: &ErrorFlags,
        stdin_file: &StdinFile,
        strip_root: Option<&std::path::Path>,
        errors: &ConcreteLocPrintableErrorSet,
        warnings: &ConcreteLocPrintableErrorSet,
        lazy_msg: Option<&str>,
    ) -> std::io::Result<()> {
        fn render_counts(err_count: usize, warn_count: usize, sep: &str) -> String {
            let error_str = if err_count != 1 { "errors" } else { "error" };
            let warning_str = if warn_count != 1 {
                "warnings"
            } else {
                "warning"
            };

            if warn_count == 0 {
                format!("{}{}{}", err_count, sep, error_str)
            } else if err_count == 0 {
                format!("{}{}{}", warn_count, sep, warning_str)
            } else {
                format!(
                    "{}{}{} and {}{}{}",
                    err_count, sep, error_str, warn_count, sep, warning_str
                )
            }
        }

        let truncate = !flags.show_all_errors;
        let max_count = if truncate { Some(50) } else { None };
        let err_count = errors.0.len();
        let warn_count = warnings.0.len();
        let error_groups = collect_errors_into_groups(max_count, errors);
        let warning_groups = collect_errors_into_groups(
            max_count.map(|max| max.saturating_sub(err_count)),
            warnings,
        );

        let total_count = err_count + warn_count;
        let mut hidden_branches = false;

        for (kind, friendly_err) in error_groups {
            let err = PrintableError(kind, friendly_err);
            let (had_hidden, styles) = get_pretty_printed_error(
                stdin_file,
                strip_root,
                flags,
                Severity::Err,
                flags.show_all_branches,
                err,
            );
            if had_hidden {
                hidden_branches = true;
            }
            print_styles(out, flags, styles)?;
        }

        // Print warnings
        for (kind, friendly_err) in warning_groups {
            let err = PrintableError(kind, friendly_err);
            let (had_hidden, styles) = get_pretty_printed_error(
                stdin_file,
                strip_root,
                flags,
                Severity::Warn,
                flags.show_all_branches,
                err,
            );
            if had_hidden {
                hidden_branches = true;
            }
            print_styles(out, flags, styles)?;
        }
        if total_count > 0 {
            writeln!(out)?;
        }
        if truncate && total_count > 50 {
            let (remaining_errs, remaining_warns) = if (err_count as i32) - 50 < 0 {
                (0, warn_count.saturating_sub(50 - err_count))
            } else {
                (err_count - 50, warn_count)
            };
            write!(
                out,
                "... {} (only 50 out of {} displayed)\n",
                render_counts(remaining_errs, remaining_warns, " more "),
                render_counts(err_count, warn_count, " ")
            )?;
            writeln!(out, "To see all errors, re-run Flow with --show-all-errors")?;
            out.flush()?;
        } else {
            writeln!(out, "Found {}", render_counts(err_count, warn_count, " "))?;
        }
        if hidden_branches {
            writeln!(
                out,
                "\nOnly showing the most relevant union/intersection branches.\nTo see all branches, re-run Flow with --show-all-branches"
            )?;
        }
        if let Some(msg) = lazy_msg {
            writeln!(out, "\n{}", msg)?;
        }
        Ok(())
    }

    fn list_files<W: std::io::Write>(
        out: &mut W,
        strip_root: Option<&std::path::Path>,
        errors: &ConcreteLocPrintableErrorSet,
        warnings: &ConcreteLocPrintableErrorSet,
    ) -> std::io::Result<()> {
        let fold_files = |error: &PrintableError<Loc>,
                          acc: &mut std::collections::BTreeSet<String>| {
            let loc = loc_of_printable_error(error);
            if let Some(source) = &loc.source {
                acc.insert(print_file_key(strip_root, Some(source)));
            }
        };

        let mut files: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        for error in errors.0.iter() {
            fold_files(error, &mut files);
        }
        for error in warnings.0.iter() {
            fold_files(error, &mut files);
        }

        for file in files {
            writeln!(out, "{}", file)?;
        }
        Ok(())
    }

    pub fn print_errors<W: std::io::Write>(
        out: &mut W,
        flags: &ErrorFlags,
        stdin_file: &StdinFile,
        strip_root: Option<&std::path::Path>,
        errors: &ConcreteLocPrintableErrorSet,
        warnings: &ConcreteLocPrintableErrorSet,
        lazy_msg: Option<&str>,
    ) -> std::io::Result<()> {
        if flags.list_files {
            list_files(out, strip_root, errors, warnings)
        } else {
            format_errors(
                out, flags, stdin_file, strip_root, errors, warnings, lazy_msg,
            )
        }
    }
}

/// JSON output
pub mod json_output {
    use std::collections::BTreeMap;
    use std::collections::BTreeSet;

    use flow_lint_settings::severity::Severity;
    use serde_json::Value;
    use serde_json::json;

    use super::*;

    fn ordered_entries(value: &Value) -> Vec<(&String, &Value)> {
        match value {
            Value::Object(props) => {
                let mut entries = props.iter().collect::<Vec<_>>();
                entries.sort_by(|(left_key, _), (right_key, _)| left_key.cmp(right_key));
                entries
            }
            _ => Vec::new(),
        }
    }

    fn json_string_of_value(value: &Value) -> String {
        match value {
            Value::Array(values) => format!(
                "[{}]",
                values
                    .iter()
                    .map(json_string_of_value)
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            Value::Object(_) => format!(
                "{{{}}}",
                ordered_entries(value)
                    .iter()
                    .map(|(key, value)| format!(
                        "{}:{}",
                        serde_json::to_string(key).unwrap(),
                        json_string_of_value(value)
                    ))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            _ => serde_json::to_string(value).unwrap(),
        }
    }

    fn json_to_multiline(value: &Value) -> String {
        fn loop_(indent: &str, value: &Value) -> String {
            let single = json_string_of_value(value);
            if single.len() < 80 {
                single
            } else {
                match value {
                    Value::Array(values) => {
                        let next_indent = format!("{indent}  ");
                        let rendered = values
                            .iter()
                            .map(|value| loop_(&next_indent, value))
                            .collect::<Vec<_>>();
                        format!(
                            "[\n{next_indent}{}\n{indent}]",
                            rendered.join(&format!(",\n{next_indent}"))
                        )
                    }
                    Value::Object(_) => {
                        let next_indent = format!("{indent}  ");
                        let rendered = ordered_entries(value)
                            .iter()
                            .map(|(key, value)| {
                                format!(
                                    "{}:{}",
                                    serde_json::to_string(key).unwrap(),
                                    loop_(&next_indent, value)
                                )
                            })
                            .collect::<Vec<_>>();
                        format!(
                            "{{\n{next_indent}{}\n{indent}}}",
                            rendered.join(&format!(",\n{next_indent}"))
                        )
                    }
                    _ => single,
                }
            }
        }

        loop_("", value)
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
    pub enum JsonVersion {
        JsonV1,
        JsonV2,
    }

    fn unwrap_message(message: &Message<Loc>) -> (&String, Option<Loc>) {
        match message {
            Message::BlameM(loc, str) if !loc.is_none() => (str, Some(loc.dupe())),
            Message::BlameM(_, str) | Message::CommentM(str) => (str, None),
        }
    }

    fn json_of_message_props(
        stdin_file: &StdinFile,
        strip_root: Option<&str>,
        offset_kind: OffsetKind,
        message: &Message<Loc>,
    ) -> Vec<(String, Value)> {
        let (desc, loc) = unwrap_message(message);
        let type_ = match message {
            Message::BlameM(_, _) => "Blame",
            Message::CommentM(_) => "Comment",
        };

        let mut props = vec![
            ("descr".to_string(), json!(desc)),
            ("type".to_string(), json!(type_)),
        ];

        match loc {
            None => {
                props.extend(deprecated_json_props_of_loc(
                    strip_root,
                    &flow_parser::loc::LOC_NONE,
                ));
            }
            Some(ref loc) => {
                let offset_table = get_offset_table_expensive(stdin_file, offset_kind, loc);
                props.push((
                    "loc".to_string(),
                    flow_common::reason::json_of_loc(strip_root, true, offset_table.as_ref(), loc),
                ));
                props.extend(deprecated_json_props_of_loc(strip_root, loc));
            }
        }

        props
    }

    // Returns the first line of the context
    fn json_of_loc_context(stdin_file: &StdinFile, loc: Option<&Loc>) -> Value {
        match loc {
            None => Value::Null,
            Some(loc) => {
                let filename = file_of_source(loc.source.as_ref());
                match filename {
                    None => Value::Null,
                    Some(filename) => match read_lines_in_file(loc, Some(&filename), stdin_file) {
                        Some((first_line, _)) => json!(first_line),
                        None => Value::Null,
                    },
                }
            }
        }
    }

    fn json_of_loc_context_abridged(
        stdin_file: &StdinFile,
        max_len: usize,
        loc: Option<&Loc>,
    ) -> Value {
        match loc {
            None => Value::Null,
            Some(loc) => {
                let filename = file_of_source(loc.source.as_ref());
                match filename {
                    None => Value::Null,
                    Some(filename) => match read_lines_in_file(loc, Some(&filename), stdin_file) {
                        // Read the lines referenced in the loc
                        Some((first_line, rest_lines)) => {
                            let mut all_lines = vec![first_line];
                            all_lines.extend(rest_lines);

                            let num_lines = all_lines.len();
                            let mut numbered_lines = serde_json::Map::new();

                            if num_lines <= max_len {
                                // There are few enough lines that we can use them all for context
                                for (i, line) in all_lines.iter().enumerate() {
                                    let line_num = (loc.start.line as usize + i).to_string();
                                    numbered_lines.insert(line_num, json!(line));
                                }
                            } else {
                                // There are too many lines for context. Let's take some lines from the
                                // start of the loc and some from the end of the loc
                                let start_len = max_len.div_ceil(2);
                                let end_len = max_len / 2; // floor

                                for (i, line) in all_lines.iter().take(start_len).enumerate() {
                                    let line_num = (loc.start.line as usize + i).to_string();
                                    numbered_lines.insert(line_num, json!(line));
                                }
                                for i in 0..end_len {
                                    let idx = num_lines - end_len + i;
                                    let line_num = (loc.start.line as usize + idx).to_string();
                                    numbered_lines.insert(line_num, json!(&all_lines[idx]));
                                }
                            }
                            Value::Object(numbered_lines)
                        }
                        None => Value::Null,
                    },
                }
            }
        }
    }

    fn json_of_loc_with_context(
        strip_root: Option<&str>,
        stdin_file: &StdinFile,
        offset_kind: OffsetKind,
        loc: &Loc,
    ) -> Value {
        let offset_table = get_offset_table_expensive(stdin_file, offset_kind, loc);
        let mut props =
            flow_common::reason::json_of_loc_props(strip_root, true, offset_table.as_ref(), loc);
        props.push((
            "context".to_string(),
            json_of_loc_context_abridged(stdin_file, 5, Some(loc)),
        ));
        Value::Object(props.into_iter().collect())
    }

    fn json_of_message_with_context(
        strip_root: Option<&str>,
        stdin_file: &StdinFile,
        offset_kind: OffsetKind,
        message: &Message<Loc>,
    ) -> Value {
        let (_, loc) = unwrap_message(message);
        let mut props: serde_json::Map<String, Value> =
            json_of_message_props(stdin_file, strip_root, offset_kind, message)
                .into_iter()
                .collect();
        props.insert(
            "context".to_string(),
            json_of_loc_context(stdin_file, loc.as_ref()),
        );
        Value::Object(props)
    }

    fn json_of_infos<F: Fn(&Message<Loc>) -> Value>(
        json_of_message: &F,
        infos: &[Info<Loc>],
    ) -> Value {
        let messages = infos_to_messages(infos.to_vec());
        Value::Array(messages.iter().map(json_of_message).collect())
    }

    fn json_of_info_tree<F: Fn(&Message<Loc>) -> Value>(
        json_of_message: &F,
        tree: &InfoTree<Loc>,
    ) -> Value {
        let (infos, kids) = match tree {
            InfoTree::InfoLeaf(infos) => (infos, None),
            InfoTree::InfoNode(infos, kids) => (infos, Some(kids)),
        };

        let mut props = vec![("message".to_string(), json_of_infos(json_of_message, infos))];

        if let Some(kids) = kids {
            let children: Vec<Value> = kids
                .iter()
                .map(|k| json_of_info_tree(json_of_message, k))
                .collect();
            props.push(("children".to_string(), Value::Array(children)));
        }

        Value::Object(props.into_iter().collect())
    }

    fn json_of_classic_error_props<F: Fn(&Message<Loc>) -> Value>(
        json_of_message: F,
        error: &ClassicError<Loc>,
    ) -> Vec<(String, Value)> {
        let messages: Vec<Value> = error.messages.iter().map(&json_of_message).collect();
        let error_codes: Vec<Value> = error
            .error_codes
            .iter()
            .map(|c| json!(c.as_str()))
            .collect();

        let mut props = vec![
            ("message".to_string(), Value::Array(messages)),
            ("error_codes".to_string(), Value::Array(error_codes)),
        ];

        // add extra if present
        if !error.extra.is_empty() {
            let extra: Vec<Value> = error
                .extra
                .iter()
                .map(|tree| json_of_info_tree(&json_of_message, tree))
                .collect();
            props.push(("extra".to_string(), Value::Array(extra)));
        }

        props
    }

    fn json_of_message_inline_friendly(inline: &friendly::MessageInline) -> Value {
        match inline {
            friendly::MessageInline::Text(text) => {
                json!({
                    "kind": "Text",
                    "text": text
                })
            }
            friendly::MessageInline::Code(code) => {
                json!({
                    "kind": "Code",
                    "text": code
                })
            }
        }
    }

    fn json_of_message_friendly(message: &friendly::Message<i32>) -> Value {
        let flattened = friendly::flatten_message(message.clone());
        let items: Vec<Value> = flattened
            .0
            .iter()
            .flat_map(|feature| match feature {
                friendly::MessageFeature::Inline(inlines) => inlines
                    .iter()
                    .map(json_of_message_inline_friendly)
                    .collect(),
                friendly::MessageFeature::Reference(inlines, id) => {
                    vec![json!({
                        "kind": "Reference",
                        "referenceId": id.to_string(),
                        "message": inlines.iter().map(json_of_message_inline_friendly).collect::<Vec<_>>()
                    })]
                }
            })
            .collect();
        Value::Array(items)
    }

    fn json_of_message_group_friendly(message_group: &friendly::MessageGroup<i32>) -> Value {
        let group_message = json_of_message_friendly(&message_group.group_message);

        let group_message_post: Vec<(&str, Value)> = match &message_group.group_message_post {
            Some(post) => vec![("post_message", json_of_message_friendly(post))],
            None => vec![],
        };

        let group_message_list: Vec<&friendly::MessageGroup<i32>> = match &message_group
            .group_message_nested
        {
            friendly::GroupMessageNested::NoNesting => vec![],
            friendly::GroupMessageNested::StackedErrorNesting(list)
            | friendly::GroupMessageNested::SpeculationErrorNesting(list) => list.iter().collect(),
        };

        if group_message_list.is_empty() && group_message_post.is_empty() {
            group_message
        } else {
            let mut props = vec![
                ("kind".to_string(), json!("UnorderedList")),
                ("message".to_string(), group_message),
                (
                    "items".to_string(),
                    Value::Array(
                        group_message_list
                            .iter()
                            .map(|g| json_of_message_group_friendly(g))
                            .collect(),
                    ),
                ),
            ];
            for (k, v) in group_message_post {
                props.push((k.to_string(), v));
            }
            Value::Object(props.into_iter().collect())
        }
    }

    fn json_of_references(
        strip_root: Option<&str>,
        stdin_file: &StdinFile,
        offset_kind: OffsetKind,
        references: &BTreeMap<i32, Loc>,
    ) -> Value {
        let mut props = serde_json::Map::new();
        for (id, loc) in references.iter() {
            props.insert(
                id.to_string(),
                json_of_loc_with_context(strip_root, stdin_file, offset_kind, loc),
            );
        }
        Value::Object(props)
    }

    fn json_of_friendly_error_props(
        strip_root: Option<&str>,
        stdin_file: &StdinFile,
        offset_kind: OffsetKind,
        error_kind: ErrorKind,
        error: &friendly::FriendlyGeneric<Loc>,
    ) -> Vec<(String, Value)> {
        let (_, primary_loc, message_group) = friendly::message_group_of_error(
            true,
            true,
            false, // Not super important for json APIs
            false,
            error_kind,
            error.clone(),
        );

        let (references, message_group) = friendly::extract_references(message_group);

        let root_loc = match &error.root {
            None => Value::Null,
            Some(root) => {
                json_of_loc_with_context(strip_root, stdin_file, offset_kind, &root.root_loc)
            }
        };

        vec![
            // Unfortunately, Nuclide currently depends on this flag. Remove it in the future?
            ("classic".to_string(), json!(false)),
            // NOTE: `primaryLoc` is the location we want to show in an IDE! `rootLoc`
            // is another loc which Flow associates with some errors. We include it
            // for tools which are interested in using the location to enhance
            // their rendering. `primaryLoc` will always be inside `rootLoc`.
            (
                "primaryLoc".to_string(),
                json_of_loc_with_context(strip_root, stdin_file, offset_kind, &primary_loc),
            ),
            ("rootLoc".to_string(), root_loc),
            // NOTE: This `messageMarkup` can be concatenated into a string when
            // implementing the LSP error output.
            (
                "messageMarkup".to_string(),
                json_of_message_group_friendly(&message_group),
            ),
            // NOTE: These `referenceLocs` can become `relatedLocations` when
            // implementing the LSP error output.
            (
                "referenceLocs".to_string(),
                json_of_references(strip_root, stdin_file, offset_kind, &references),
            ),
        ]
    }

    fn json_of_error_props(
        strip_root: Option<&str>,
        stdin_file: &StdinFile,
        version: JsonVersion,
        offset_kind: OffsetKind,
        json_of_message: impl Fn(&Message<Loc>) -> Value,
        severity: Severity,
        suppression_locs: &BTreeSet<Loc>,
        error: &PrintableError<Loc>,
    ) -> Vec<(String, Value)> {
        let PrintableError(kind, friendly_error) = error;

        fn kind_str(kind: ErrorKind) -> &'static str {
            match kind {
                ErrorKind::ParseError => "parse",
                // We report this as a parse error even though it wasn't produced by the parser
                ErrorKind::PseudoParseError => "parse",
                ErrorKind::InferError => "infer",
                // "InferWarning"s should still really be treated as errors. (The name is outdated.)
                ErrorKind::InferWarning(_) => "infer",
                ErrorKind::InternalError => "internal",
                ErrorKind::DuplicateProviderError => "duplicate provider",
                ErrorKind::RecursionLimitError => "recursion limit exceeded",
                ErrorKind::LintError(_) => "lint",
            }
        }

        fn severity_str(severity: Severity) -> &'static str {
            match severity {
                Severity::Err => "error",
                Severity::Warn => "warning",
                Severity::Off => "off",
            }
        }

        let kind_str = kind_str(*kind);
        let severity_str = severity_str(severity);

        let suppressions: Vec<Value> = suppression_locs
            .iter()
            .map(|loc| {
                let offset_table = get_offset_table_expensive(stdin_file, offset_kind, loc);
                json!({
                    "loc": flow_common::reason::json_of_loc(
                        strip_root,
                        true,
                        offset_table.as_ref(),
                        loc,
                    )
                })
            })
            .collect();

        let mut props = vec![
            ("kind".to_string(), json!(kind_str)),
            ("level".to_string(), json!(severity_str)),
            ("suppressions".to_string(), Value::Array(suppressions)),
        ];

        // add the error type specific props
        match version {
            JsonVersion::JsonV1 => {
                let classic = friendly::to_classic(*kind, friendly_error.clone());
                props.extend(json_of_classic_error_props(json_of_message, &classic));
            }
            JsonVersion::JsonV2 => {
                props.extend(json_of_friendly_error_props(
                    strip_root,
                    stdin_file,
                    offset_kind,
                    *kind,
                    friendly_error,
                ));
            }
        }

        props
    }

    fn json_of_error_with_context(
        strip_root: Option<&str>,
        stdin_file: &StdinFile,
        version: JsonVersion,
        offset_kind: OffsetKind,
        severity: Severity,
        error: &PrintableError<Loc>,
        suppression_locs: &BTreeSet<Loc>,
    ) -> Value {
        let json_of_message =
            |m: &Message<Loc>| json_of_message_with_context(strip_root, stdin_file, offset_kind, m);
        let props = json_of_error_props(
            strip_root,
            stdin_file,
            version,
            offset_kind,
            json_of_message,
            severity,
            suppression_locs,
            error,
        );
        Value::Object(props.into_iter().collect())
    }

    pub fn json_of_errors_with_context(
        strip_root: Option<&str>,
        stdin_file: &StdinFile,
        suppressed_errors: &[(PrintableError<Loc>, BTreeSet<Loc>)],
        version: JsonVersion,
        offset_kind: OffsetKind,
        errors: &ConcreteLocPrintableErrorSet,
        warnings: &ConcreteLocPrintableErrorSet,
    ) -> Value {
        let empty_suppressions = BTreeSet::new();

        let f =
            |severity: Severity, error: &PrintableError<Loc>, suppression_locs: &BTreeSet<Loc>| {
                json_of_error_with_context(
                    strip_root,
                    stdin_file,
                    version,
                    offset_kind,
                    severity,
                    error,
                    suppression_locs,
                )
            };

        let mut obj_props_rev: Vec<Value> = Vec::new();

        for error in errors.0.iter() {
            obj_props_rev.push(f(Severity::Err, error, &empty_suppressions));
        }

        for warning in warnings.0.iter() {
            obj_props_rev.push(f(Severity::Warn, warning, &empty_suppressions));
        }

        // Add suppressed errors (they show up as "suppressed error"s, not "suppressed off"s)
        for (err, suppressions) in suppressed_errors {
            obj_props_rev.push(f(Severity::Err, err, suppressions));
        }

        Value::Array(obj_props_rev)
    }

    pub fn full_status_json_of_errors(
        strip_root: Option<&str>,
        suppressed_errors: &[(PrintableError<Loc>, BTreeSet<Loc>)],
        version: JsonVersion,
        stdin_file: &StdinFile,
        offset_kind: OffsetKind,
        errors: &ConcreteLocPrintableErrorSet,
        warnings: &ConcreteLocPrintableErrorSet,
    ) -> impl Fn(Vec<(String, Value)>) -> Value + use<> {
        // This function has an unusual signature because the first part can be
        // expensive -- specifically `json_of_errors_with_context` can take a while,
        // and we would like to include the time spent in our profiling data.
        //
        // However, that profiling data is also included in the output. This function
        // is designed to be partially applied, with the partial application
        // performing the expensive work within a running profiling segment. The
        // returned closure can be passed the finished profiling data.
        let errors_json = json_of_errors_with_context(
            strip_root,
            stdin_file,
            suppressed_errors,
            version,
            offset_kind,
            errors,
            warnings,
        );

        let json_version = match version {
            JsonVersion::JsonV1 => "1",
            JsonVersion::JsonV2 => "2",
        };

        let props = vec![
            (
                "flowVersion".to_string(),
                json!(flow_common::flow_version::VERSION),
            ),
            ("jsonVersion".to_string(), json!(json_version)),
            ("errors".to_string(), errors_json),
            ("passed".to_string(), json!(errors.0.is_empty())),
        ];

        move |profiling_props: Vec<(String, Value)>| {
            let mut all_props = props.clone();
            all_props.extend(profiling_props);
            Value::Object(all_props.into_iter().collect())
        }
    }

    pub fn format_errors<W: std::io::Write>(
        out: &mut W,
        strip_root: Option<&str>,
        suppressed_errors: &[(PrintableError<Loc>, BTreeSet<Loc>)],
        pretty: bool,
        version: JsonVersion,
        stdin_file: &StdinFile,
        offset_kind: OffsetKind,
        errors: &ConcreteLocPrintableErrorSet,
        warnings: &ConcreteLocPrintableErrorSet,
    ) -> impl FnOnce(Vec<(String, Value)>) -> std::io::Result<()> {
        let get_json = full_status_json_of_errors(
            strip_root,
            suppressed_errors,
            version,
            stdin_file,
            offset_kind,
            errors,
            warnings,
        );

        move |profiling_props: Vec<(String, Value)>| {
            let res = get_json(profiling_props);
            if pretty {
                write!(out, "{}", json_to_multiline(&res))?;
            } else {
                write!(out, "{}", json_string_of_value(&res))?;
            }
            out.flush()
        }
    }

    pub fn print_errors<W: std::io::Write>(
        out: &mut W,
        strip_root: Option<&str>,
        suppressed_errors: &[(PrintableError<Loc>, BTreeSet<Loc>)],
        pretty: bool,
        version: JsonVersion,
        stdin_file: &StdinFile,
        errors: &ConcreteLocPrintableErrorSet,
        warnings: &ConcreteLocPrintableErrorSet,
    ) -> std::io::Result<()> {
        print_errors_with_offset_kind(
            out,
            strip_root,
            suppressed_errors,
            pretty,
            version,
            stdin_file,
            OffsetKind::Utf8,
            errors,
            warnings,
        )
    }

    pub fn print_errors_with_offset_kind<W: std::io::Write>(
        out: &mut W,
        strip_root: Option<&str>,
        suppressed_errors: &[(PrintableError<Loc>, BTreeSet<Loc>)],
        pretty: bool,
        version: JsonVersion,
        stdin_file: &StdinFile,
        offset_kind: OffsetKind,
        errors: &ConcreteLocPrintableErrorSet,
        warnings: &ConcreteLocPrintableErrorSet,
    ) -> std::io::Result<()> {
        let format_fn = format_errors(
            out,
            strip_root,
            suppressed_errors,
            pretty,
            version,
            stdin_file,
            offset_kind,
            errors,
            warnings,
        );
        format_fn(vec![])
    }
}

pub mod vim_emacs_output {
    use super::*;

    pub fn string_of_loc(strip_root: Option<&str>, loc: &Loc) -> String {
        match loc.source.as_ref() {
            None => String::new(),
            Some(source) => {
                let file = flow_common::reason::string_of_source(strip_root, source);
                let line = loc.start.line;
                let start = loc.start.column + 1;
                let end_ = loc.end.column;

                if line <= 0 {
                    format!("File \"{}\", line 0", file)
                } else if line == loc.end.line && start - end_ == 1 {
                    format!("File \"{}\", line {}, character {}", file, line, start)
                } else {
                    format!(
                        "File \"{}\", line {}, characters {}-{}",
                        file, line, start, end_
                    )
                }
            }
        }
    }

    pub fn print_errors<W: std::io::Write>(
        strip_root: Option<&str>,
        oc: &mut W,
        errors: &ConcreteLocPrintableErrorSet,
        warnings: &ConcreteLocPrintableErrorSet,
    ) -> std::io::Result<()> {
        fn endline(s: &str) -> String {
            if s.is_empty() {
                String::new()
            } else {
                format!("{}\n", s)
            }
        }

        fn to_pp_string(strip_root: Option<&str>, prefix: &str, message: &Message<Loc>) -> String {
            let (loc, msg) = to_pp(message);
            let loc_str = string_of_loc(strip_root, loc);
            format!("{}{}{}", endline(&loc_str), prefix, endline(msg))
        }

        fn classic_to_string(
            strip_root: Option<&str>,
            prefix: &str,
            error: &ClassicError<Loc>,
        ) -> String {
            let messages = &error.messages;
            let mut buf = String::new();

            match messages.as_slice() {
                [] => panic!("classic_to_string: empty messages"),
                [message1, rest_of_error @ ..] => {
                    buf.push_str(&to_pp_string(strip_root, prefix, message1));
                    for message in rest_of_error {
                        buf.push_str(&to_pp_string(strip_root, "", message));
                    }
                }
            }

            buf
        }

        fn to_string(
            strip_root: Option<&str>,
            prefix: &str,
            error: &PrintableError<Loc>,
        ) -> String {
            let PrintableError(error_kind, friendly_error) = error;
            let classic = friendly::to_classic(*error_kind, friendly_error.clone());
            classic_to_string(strip_root, prefix, &classic)
        }

        let mut sl: Vec<String> = Vec::new();
        for err in errors.0.iter() {
            sl.push(to_string(strip_root, "Error: ", err));
        }
        for warn in warnings.0.iter() {
            sl.push(to_string(strip_root, "Warning: ", warn));
        }
        sl.sort();
        sl.dedup();
        for s in sl {
            writeln!(oc, "{}", s)?;
        }

        oc.flush()?;
        Ok(())
    }
}

pub mod lsp_output {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct LspDiagnostic {
        /// The file+range at which the message applies
        pub loc: Loc,
        /// The diagnostic's message
        pub message: String,
        /// An error code
        pub code: String,
        pub related_locations: Vec<(Loc, String)>,
    }

    pub fn lsp_of_error(
        has_detailed_diagnostics: bool,
        error: &PrintableError<Loc>,
    ) -> LspDiagnostic {
        // Example: "Error about `code` in type Ref(`foo`)"
        // will produce LSP message "Error about `code` in type `foo` [1]"
        // and the LSP related location will have message "[1]: `foo`"
        let PrintableError(kind, friendly_generic) = error;

        let (hidden_branch, loc, group) = friendly::message_group_of_error(
            true,
            false,
            true,
            false,
            *kind,
            friendly_generic.clone(),
        );

        let (references, group) = friendly::extract_references(group);
        let features = friendly::indented_message_of_group_message(true, group);

        // Build up the message and related locations
        let mut message = String::new();
        let mut related_locations: Vec<(Loc, String)> = Vec::new();

        for feature in features.0.iter() {
            match feature {
                friendly::MessageFeature::Inline(inlines) => {
                    message.push_str(&friendly::string_of_message_inlines(inlines));
                }
                friendly::MessageFeature::Reference(inlines, id) => {
                    let ref_id = id.to_string();
                    let ref_text = friendly::string_of_message_inlines(inlines);
                    if let Some(ref_loc) = references.get(id) {
                        let ref_message = format!("[{}] {}", ref_id, ref_text);
                        message.push_str(&format!(" {} [{}]", ref_text, ref_id));
                        related_locations.push((ref_loc.dupe(), ref_message));
                    }
                }
            }
        }

        if has_detailed_diagnostics && hidden_branch.is_some() {
            message.push_str("\n\nOnly showing the most relevant union/intersection branches.");
            message.push_str("\nClick \"Click for full error\" to see the full error message.");
        }

        LspDiagnostic {
            loc,
            message: message.trim().to_string(),
            code: string_of_error_code(code_of_printable_error(error), *kind),
            related_locations: related_locations.into_iter().collect(),
        }
    }
}
