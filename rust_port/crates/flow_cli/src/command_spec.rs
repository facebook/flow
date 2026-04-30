/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::sync::Arc;

#[derive(Debug)]
pub struct ShowHelp;

#[derive(Clone, Debug)]
pub struct FailedToParse {
    pub msg: String,
    pub arg: String,
    pub details: Option<String>,
}

pub mod arg_spec {
    use super::*;

    pub type Values = BTreeMap<String, Vec<String>>;

    // consumes all the remaining args verbatim, to pass to a subcommand
    #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
    pub enum FlagArgCount {
        Truthy,
        NoArg,
        Arg,
        ArgList,
        ArgRest,
        ArgCommand,
        MaybeArg,
    }

    type ParseFn<T> =
        dyn Fn(&str, Option<&[String]>) -> Result<T, FailedToParse> + Send + Sync + 'static;

    #[derive(Clone)]
    pub struct FlagType<T> {
        parse: Arc<ParseFn<T>>,
        arg: FlagArgCount,
    }

    impl<T> FlagType<T> {
        fn new(
            arg: FlagArgCount,
            parse: impl Fn(&str, Option<&[String]>) -> Result<T, FailedToParse> + Send + Sync + 'static,
        ) -> Self {
            Self {
                parse: Arc::new(parse),
                arg,
            }
        }

        pub fn parse(&self, name: &str, value: Option<&[String]>) -> Result<T, FailedToParse> {
            (self.parse)(name, value)
        }

        pub fn arg_count(&self) -> FlagArgCount {
            self.arg
        }
    }

    #[derive(Clone, Debug)]
    pub struct FlagMetadata {
        pub doc: String,
        pub env: Option<String>,
        pub arg_count: FlagArgCount,
    }

    pub fn string() -> FlagType<Option<String>> {
        FlagType::new(FlagArgCount::Arg, |_name, value| {
            Ok(match value {
                Some([x]) => Some(x.to_string()),
                _ => None,
            })
        })
    }

    pub fn bool_flag() -> FlagType<Option<bool>> {
        FlagType::new(FlagArgCount::Arg, |name, value| {
            Ok(match value {
                Some([x]) if x == "0" || x == "false" => Some(false),
                None => Some(false),
                Some([x]) if x == "1" || x == "true" => Some(true),
                Some(_) => {
                    return Err(FailedToParse {
                        arg: name.to_string(),
                        msg: "Invalid argument".to_string(),
                        details: Some("expected one of: true, false, 0, 1".to_string()),
                    });
                }
            })
        })
    }

    pub fn int() -> FlagType<Option<i32>> {
        FlagType::new(FlagArgCount::Arg, |name, value| {
            Ok(match value {
                Some([x]) => Some(x.parse::<i32>().map_err(|_| FailedToParse {
                    arg: name.to_string(),
                    msg: "Invalid argument".to_string(),
                    details: Some(format!("expected an integer, got {:?}", x)),
                })?),
                _ => None,
            })
        })
    }

    pub fn uint() -> FlagType<Option<i32>> {
        FlagType::new(FlagArgCount::Arg, |name, value| {
            Ok(match value {
                Some([x]) => {
                    let i = x.parse::<i32>().map_err(|_| FailedToParse {
                        arg: name.to_string(),
                        msg: "Invalid argument".to_string(),
                        details: Some(format!("expected an unsigned integer, got {:?}", x)),
                    })?;
                    if i < 0 {
                        return Err(FailedToParse {
                            arg: name.to_string(),
                            msg: "Invalid argument".to_string(),
                            details: Some(format!("expected an unsigned integer, got {:?}", x)),
                        });
                    }
                    Some(i)
                }
                _ => None,
            })
        })
    }

    pub fn enum_flag<T: Clone + Send + Sync + 'static>(
        values: Vec<(&'static str, T)>,
    ) -> FlagType<Option<T>> {
        FlagType::new(FlagArgCount::Arg, move |name, value| {
            Ok(match value {
                Some([x]) => {
                    let value = values
                        .iter()
                        .find(|(s, _)| *s == x)
                        .map(|(_, v)| v.clone())
                        .ok_or_else(|| FailedToParse {
                            arg: name.to_string(),
                            msg: "Invalid argument".to_string(),
                            details: Some(format!(
                                "expected one of: {}",
                                values
                                    .iter()
                                    .map(|(s, _)| *s)
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            )),
                        })?;
                    Some(value)
                }
                _ => None,
            })
        })
    }

    pub fn command_flag<T: Clone + Send + Sync + 'static>(
        cmds: Vec<(&'static str, T)>,
    ) -> FlagType<Option<(T, Vec<String>)>> {
        let enum_type = enum_flag(cmds);
        FlagType::new(FlagArgCount::ArgCommand, move |name, value| {
            Ok(match value {
                Some([cmd_name, rest @ ..]) => enum_type
                    .parse(name, Some(std::slice::from_ref(cmd_name)))?
                    .map(|cmd| (cmd, rest.iter().map(|s| s.to_string()).collect())),
                Some([]) | None => None,
            })
        })
    }

    pub fn truthy() -> FlagType<bool> {
        FlagType::new(FlagArgCount::Truthy, |_name, value| Ok(value.is_some()))
    }

    pub fn no_arg() -> FlagType<Option<bool>> {
        FlagType::new(FlagArgCount::NoArg, |_name, value| {
            Ok(match value {
                None => None,
                Some([x]) if x == "false" => Some(false),
                Some(_) => Some(true),
            })
        })
    }

    pub fn required<T: Clone + Send + Sync + 'static>(
        default: Option<T>,
        arg_type: FlagType<Option<T>>,
    ) -> FlagType<T> {
        FlagType::new(arg_type.arg_count(), move |name, value| match value {
            None => match &default {
                Some(default) => Ok(default.clone()),
                None => Err(FailedToParse {
                    arg: name.to_string(),
                    msg: "Missing required arguments".to_string(),
                    details: None,
                }),
            },
            value => match arg_type.parse(name, value)? {
                Some(result) => Ok(result),
                None => Err(FailedToParse {
                    arg: name.to_string(),
                    msg: "Wrong type for required argument".to_string(),
                    details: value.and_then(|value| match value {
                        [x] => Some(format!("got {}", x)),
                        _ => None,
                    }),
                }),
            },
        })
    }

    pub fn optional<T: Send + Sync + 'static>(
        arg_type: FlagType<Option<T>>,
    ) -> FlagType<Option<T>> {
        FlagType::new(arg_type.arg_count(), move |name, value| match value {
            None => Ok(None),
            value => arg_type.parse(name, value),
        })
    }

    pub fn list_of<T: Send + Sync + 'static>(
        arg_type: FlagType<Option<T>>,
    ) -> FlagType<Option<Vec<T>>> {
        FlagType::new(FlagArgCount::ArgList, move |name, value| match value {
            None => Ok(Some(vec![])),
            Some(values) => Ok(Some(
                values
                    .iter()
                    .map(|raw_value| {
                        arg_type
                            .parse(name, Some(std::slice::from_ref(raw_value)))
                            .and_then(|value| {
                                value.ok_or_else(|| FailedToParse {
                                    arg: name.to_string(),
                                    msg: "Invalid argument".to_string(),
                                    details: Some(format!(
                                        "wrong type for argument list item: {}",
                                        raw_value
                                    )),
                                })
                            })
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )),
        })
    }

    pub fn delimited<T: Send + Sync + 'static>(
        delim: &'static str,
        arg_type: FlagType<Option<T>>,
    ) -> FlagType<Option<Vec<T>>> {
        FlagType::new(FlagArgCount::Arg, move |name, value| {
            Ok(match value {
                Some([x]) => {
                    let args: Vec<&str> = x.split(delim).filter(|s| !s.is_empty()).collect();
                    Some(
                        args.into_iter()
                            .map(|arg| {
                                let arg_str = arg.to_string();
                                match arg_type.parse(name, Some(std::slice::from_ref(&arg_str)))? {
                                    None => Err(FailedToParse {
                                        arg: name.to_string(),
                                        msg: "Invalid argument".to_string(),
                                        details: Some(format!("wrong type for value: {}", arg)),
                                    }),
                                    Some(result) => Ok(result),
                                }
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    )
                }
                _ => None,
            })
        })
    }

    pub fn key_value<K: Send + Sync + 'static, V: Send + Sync + 'static>(
        delim: &'static str,
        key_type: FlagType<Option<K>>,
        value_type: FlagType<V>,
    ) -> FlagType<Option<(K, V)>> {
        FlagType::new(FlagArgCount::Arg, move |name, value| {
            Ok(match value {
                Some([x]) => {
                    let (key_str, val_input) =
                        match x.splitn(2, delim).collect::<Vec<_>>().as_slice() {
                            [key, value] => (key.to_string(), Some(vec![value.to_string()])),
                            [key] => (key.to_string(), None),
                            _ => {
                                return Err(FailedToParse {
                                    arg: name.to_string(),
                                    msg: "Unexpected value".to_string(),
                                    details: Some(format!("got {}", x)),
                                });
                            }
                        };
                    let key = match key_type.parse(name, Some(std::slice::from_ref(&key_str)))? {
                        None => {
                            return Err(FailedToParse {
                                arg: name.to_string(),
                                msg: "Invalid argument".to_string(),
                                details: Some(format!("wrong type for key: {}", key_str)),
                            });
                        }
                        Some(result) => result,
                    };
                    let value = value_type.parse(name, val_input.as_deref())?;
                    Some((key, value))
                }
                _ => None,
            })
        })
    }

    pub fn optional_value_with_default<T: Clone + Send + Sync + 'static>(
        default: T,
        arg_type: FlagType<Option<T>>,
    ) -> FlagType<Option<T>> {
        FlagType::new(FlagArgCount::MaybeArg, move |name, value| match value {
            None => Ok(None),
            Some([]) => Ok(Some(default.clone())),
            value => match arg_type.parse(name, value)? {
                None => Err(FailedToParse {
                    arg: name.to_string(),
                    msg: "Wrong type for required argument".to_string(),
                    details: value.and_then(|v| match v {
                        [x] => Some(format!("got {}", x)),
                        _ => None,
                    }),
                }),
                Some(result) => Ok(Some(result)),
            },
        })
    }

    pub fn rest() -> FlagType<Vec<String>> {
        FlagType::new(FlagArgCount::ArgRest, |_name, value| {
            Ok(value.map_or_else(Vec::new, |values| values.to_vec()))
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Internal,
    Experimental,
}

pub struct Spec {
    pub name: String,
    pub doc: String,
    pub visibility: Visibility,
    pub usage: String,
    pub flags: BTreeMap<String, arg_spec::FlagMetadata>,
    pub anons: Vec<(String, arg_spec::FlagArgCount)>,
}

impl Spec {
    pub fn new(name: &str, doc: &str, visibility: Visibility, usage: String) -> Self {
        let mut flags = BTreeMap::new();
        flags.insert(
            "--help".to_string(),
            arg_spec::FlagMetadata {
                doc: "This list of options".to_string(),
                env: None,
                arg_count: arg_spec::FlagArgCount::Truthy,
            },
        );
        Self {
            name: name.to_string(),
            doc: doc.to_string(),
            visibility,
            usage,
            flags,
            anons: vec![],
        }
    }

    pub fn flag<T>(
        mut self,
        name: &str,
        arg_type: &arg_spec::FlagType<T>,
        doc: &str,
        env: Option<&str>,
    ) -> Self {
        self.flags.insert(
            name.to_string(),
            arg_spec::FlagMetadata {
                doc: doc.to_string(),
                env: env.map(|env| env.to_string()),
                arg_count: arg_type.arg_count(),
            },
        );
        self
    }

    pub fn anon<T>(mut self, name: &str, arg_type: &arg_spec::FlagType<T>) -> Self {
        self.anons.push((name.to_string(), arg_type.arg_count()));
        self
    }
}

pub struct Command {
    cmdname: String,
    cmddoc: String,
    cmdvisibility: Visibility,
    flags: BTreeMap<String, arg_spec::FlagMetadata>,
    anons: Vec<(String, arg_spec::FlagArgCount)>,
    string_of_usage: Box<dyn Fn() -> String + Send + Sync>,
    main: Box<dyn Fn(&arg_spec::Values) + Send + Sync>,
}

fn no_dashes(opt: &str) -> &str {
    if !opt.starts_with('-') {
        opt
    } else if !opt.starts_with("--") {
        &opt[1..]
    } else {
        &opt[2..]
    }
}

fn is_arg(arg: &str) -> bool {
    arg.len() > 1 && arg != "--" && arg.starts_with('-')
}

fn consume_args(args: &[String]) -> (Vec<String>, Vec<String>) {
    let mut is_done = false;
    let mut consumed = Vec::new();
    let mut remaining = Vec::new();
    for value in args {
        if !is_done && is_arg(value) {
            is_done = true;
        }
        if !is_done {
            consumed.push(value.clone());
        } else {
            remaining.push(value.clone());
        }
    }
    (consumed, remaining)
}

/// [find_flag "--foo" flags] looks up the spec for the "--foo" flag.
/// If the flag starts with "--no-" and there isn't an explicit flag
/// by that name, then the flag without "no-" is looked up.
///
/// Returns None if [name] is not a valid flag.
pub(crate) fn find_flag<'a>(
    name: &str,
    flags: &'a BTreeMap<String, arg_spec::FlagMetadata>,
) -> Option<(String, &'a arg_spec::FlagMetadata)> {
    if let Some(flag) = flags.get(name) {
        return Some((name.to_string(), flag));
    }
    if let Some(rest) = name.strip_prefix("--no-") {
        let positive_name = format!("--{}", rest);
        if let Some(flag) = flags.get(&positive_name) {
            if flag.arg_count == arg_spec::FlagArgCount::NoArg {
                return Some((positive_name, flag));
            }
        }
    }
    None
}

fn parse(
    values: arg_spec::Values,
    spec: &Spec,
    anon_index: &mut usize,
    argv: &[String],
) -> Result<arg_spec::Values, FailedToParse> {
    if argv.is_empty() {
        return Ok(values);
    }
    let arg = &argv[0];
    let args = &argv[1..];
    if is_arg(arg) {
        // split "--foo=bar"::args into "--foo"::"bar"::args
        let (arg_name, rest) = match arg.splitn(2, '=').collect::<Vec<_>>().as_slice() {
            [a, v] => {
                let mut new_args = vec![v.to_string()];
                new_args.extend(args.iter().cloned());
                (a.to_string(), new_args)
            }
            [a] => (a.to_string(), args.to_vec()),
            _ => unreachable!(),
        };
        parse_flag(values, spec, anon_index, &arg_name, &rest)
    } else {
        parse_anon(values, spec, anon_index, arg, args)
    }
}

fn parse_flag(
    mut values: arg_spec::Values,
    spec: &Spec,
    anon_index: &mut usize,
    arg: &str,
    args: &[String],
) -> Result<arg_spec::Values, FailedToParse> {
    let flags = &spec.flags;
    let (arg_prime, flag) = find_flag(arg, flags).ok_or_else(|| FailedToParse {
        arg: arg.to_string(),
        msg: "Unknown option".to_string(),
        details: None,
    })?;
    match flag.arg_count {
        arg_spec::FlagArgCount::Truthy => {
            values.insert(arg.to_string(), vec!["true".to_string()]);
            parse(values, spec, anon_index, args)
        }
        arg_spec::FlagArgCount::NoArg => {
            let value = if arg == arg_prime { "true" } else { "false" };
            values.insert(arg_prime, vec![value.to_string()]);
            parse(values, spec, anon_index, args)
        }
        arg_spec::FlagArgCount::Arg => match args {
            [] => Err(FailedToParse {
                arg: arg.to_string(),
                msg: "Option needs an argument".to_string(),
                details: None,
            }),
            [value, rest @ ..] => {
                if is_arg(value) {
                    return Err(FailedToParse {
                        arg: arg.to_string(),
                        msg: "Option needs an argument".to_string(),
                        details: None,
                    });
                }
                values.insert(arg.to_string(), vec![value.clone()]);
                parse(values, spec, anon_index, rest)
            }
        },
        arg_spec::FlagArgCount::ArgList => {
            let (value_list, remaining) = consume_args(args);
            values.insert(arg.to_string(), value_list);
            parse(values, spec, anon_index, &remaining)
        }
        arg_spec::FlagArgCount::MaybeArg => match args {
            [] => {
                values.insert(arg.to_string(), vec![]);
                parse(values, spec, anon_index, args)
            }
            [value, rest @ ..] => {
                let remaining = if is_arg(value) {
                    values.insert(arg.to_string(), vec![]);
                    args
                } else {
                    values.insert(arg.to_string(), vec![value.clone()]);
                    rest
                };
                parse(values, spec, anon_index, remaining)
            }
        },
        arg_spec::FlagArgCount::ArgRest => {
            panic!("Not supported");
        }
        arg_spec::FlagArgCount::ArgCommand => {
            panic!("Not supported");
        }
    }
}

fn parse_anon(
    mut values: arg_spec::Values,
    spec: &Spec,
    anon_index: &mut usize,
    arg: &str,
    args: &[String],
) -> Result<arg_spec::Values, FailedToParse> {
    let anon = spec.anons.get(*anon_index).cloned();
    match anon {
        Some((name, arg_spec::FlagArgCount::Arg)) => {
            *anon_index += 1;
            values.insert(name, vec![arg.to_string()]);
            parse(values, spec, anon_index, args)
        }
        Some((name, arg_spec::FlagArgCount::ArgList)) => {
            *anon_index += 1;
            let mut combined = vec![arg.to_string()];
            combined.extend(args.iter().cloned());
            let (value_list, remaining) = consume_args(&combined);
            values.insert(name, value_list);
            parse(values, spec, anon_index, &remaining)
        }
        Some((name, arg_spec::FlagArgCount::ArgRest)) => {
            *anon_index += 1;
            let rest = if arg == "--" {
                args.to_vec()
            } else {
                let mut v = vec![arg.to_string()];
                v.extend(args.iter().cloned());
                v
            };
            values.insert(name, rest);
            Ok(values)
        }
        Some((name, arg_spec::FlagArgCount::ArgCommand)) => {
            *anon_index += 1;
            let mut cmd = vec![arg.to_string()];
            cmd.extend(args.iter().cloned());
            values.insert(name, cmd);
            Ok(values)
        }
        Some((_, arg_spec::FlagArgCount::Truthy)) => unreachable!(),
        Some((_, arg_spec::FlagArgCount::NoArg)) => unreachable!(),
        Some((_, arg_spec::FlagArgCount::MaybeArg)) => unreachable!(),
        None => Err(FailedToParse {
            arg: arg.to_string(),
            msg: "Unexpected argument".to_string(),
            details: None,
        }),
    }
}

fn init_from_env(spec: &Spec) -> arg_spec::Values {
    let mut values = arg_spec::Values::new();
    for (arg, flag) in &spec.flags {
        match &flag.env {
            Some(env) => match std::env::var(env) {
                Ok(value) if !value.is_empty() => {
                    values.insert(arg.clone(), vec![value]);
                }
                Ok(_) | Err(_) => {}
            },
            None => {}
        }
    }
    values
}

fn parse_argv(spec: &Spec, argv: &[String]) -> Result<arg_spec::Values, FailedToParse> {
    let values = init_from_env(spec);
    let mut anon_index = 0;
    parse(values, spec, &mut anon_index, argv)
}

/// If no `col_width` is passed, the length of the longest string of the first column will be used.
/// `col_pad` will be used as additional padding between the two columns.
pub(crate) fn format_two_columns(
    margin: Option<usize>,
    col_width: Option<usize>,
    col_pad: usize,
    rows: &[(String, String)],
) -> String {
    let margin = margin.unwrap_or(100);
    let col_width =
        col_pad + col_width.unwrap_or_else(|| rows.iter().map(|(a, _)| a.len()).max().unwrap_or(0));
    let mut out = String::new();
    for (index, (name, doc)) in rows.iter().enumerate() {
        if index > 0 {
            out.push('\n');
        }
        let mut line = format!("  {:width$} ", name, width = col_width);
        if line.len() + doc.len() <= margin {
            line.push_str(doc);
            out.push_str(&line);
            continue;
        }
        out.push_str(&line);
        let indent = " ".repeat(col_width + 3);
        let mut current_len = line.len();
        for token in doc.split(' ') {
            let token_len = token.len();
            if current_len > indent.len() && current_len + 1 + token_len > margin {
                out.push('\n');
                out.push_str(&indent);
                out.push_str(token);
                current_len = indent.len() + token_len;
            } else {
                if current_len > line.len() && current_len != indent.len() {
                    out.push(' ');
                    current_len += 1;
                }
                out.push_str(token);
                current_len += token_len;
            }
        }
    }
    out
}

fn usage_string(spec: &Spec) -> String {
    let mut flags = spec
        .flags
        .iter()
        .map(|(name, meta)| (name.clone(), meta.clone()))
        .collect::<Vec<_>>();
    flags.sort_by(|(a, _), (b, _)| no_dashes(a).cmp(no_dashes(b)));
    let rows = flags
        .into_iter()
        .filter(|(_, meta)| !meta.doc.is_empty())
        .map(|(name, meta)| (name, meta.doc))
        .collect::<Vec<_>>();
    let flag_usage = format_two_columns(None, None, 0, &rows);
    format!("{}\n{}", spec.usage, flag_usage)
}

pub(crate) fn usage(spec: &Spec) {
    println!("{}", usage_string(spec));
}

pub fn command(spec: Spec, main: impl Fn(&arg_spec::Values) + Send + Sync + 'static) -> Command {
    let cmdname = spec.name.clone();
    let cmddoc = spec.doc.clone();
    let cmdvisibility = spec.visibility;
    let flags = spec.flags.clone();
    let anons = spec.anons.clone();
    let usage = usage_string(&spec);
    Command {
        cmdname,
        cmddoc,
        cmdvisibility,
        flags,
        anons,
        string_of_usage: Box::new(move || usage.clone()),
        main: Box::new(main),
    }
}

impl Command {
    pub fn name(&self) -> &str {
        &self.cmdname
    }

    pub fn doc(&self) -> &str {
        &self.cmddoc
    }

    pub fn visibility(&self) -> Visibility {
        self.cmdvisibility
    }

    #[allow(dead_code)]
    pub fn flags(&self) -> &BTreeMap<String, arg_spec::FlagMetadata> {
        &self.flags
    }

    pub(crate) fn run(&self, args: &arg_spec::Values) {
        (self.main)(args);
    }

    pub(crate) fn args_of_argv(&self, argv: &[String]) -> Result<arg_spec::Values, FailedToParse> {
        let spec = Spec {
            name: self.cmdname.clone(),
            doc: self.cmddoc.clone(),
            visibility: self.visibility(),
            usage: (self.string_of_usage)(),
            flags: self.flags.clone(),
            anons: self.anons.clone(),
        };
        parse_argv(&spec, argv)
    }

    pub(crate) fn string_of_usage(&self) -> String {
        (self.string_of_usage)()
    }
}

pub fn get<T>(
    values: &arg_spec::Values,
    name: &str,
    arg_type: &arg_spec::FlagType<T>,
) -> Result<T, FailedToParse> {
    arg_type.parse(name, values.get(name).map(Vec::as_slice))
}

pub(crate) fn parse_or_show_help(
    command: &Command,
    argv: &[String],
) -> Result<Result<arg_spec::Values, ShowHelp>, FailedToParse> {
    let args = command.args_of_argv(argv)?;
    let help = get(&args, "--help", &arg_spec::truthy())?;
    if help {
        Ok(Err(ShowHelp))
    } else {
        Ok(Ok(args))
    }
}

pub(crate) fn error_message(error: &FailedToParse) -> String {
    let mut message = format!("{}: {}", error.msg, error.arg);
    if let Some(details) = &error.details {
        write!(message, " ({details})").unwrap();
    }
    message
}
