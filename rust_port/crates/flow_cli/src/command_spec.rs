/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::sync::Arc;

pub(crate) type Values = BTreeMap<String, Vec<String>>;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum FlagArgCount {
    Truthy,
    NoArg,
    Arg,
    ArgList,
    ArgCommand,
}

#[derive(Clone, Debug)]
pub(crate) struct FlagMetadata {
    pub doc: String,
    pub env: Option<String>,
    pub arg_count: FlagArgCount,
}

#[derive(Debug)]
pub(crate) struct ShowHelp;

#[derive(Clone, Debug)]
pub(crate) struct FailedToParse {
    pub msg: String,
    pub arg: String,
    pub details: Option<String>,
}

type ParseFn<T> =
    dyn Fn(&str, Option<&[String]>) -> Result<T, FailedToParse> + Send + Sync + 'static;

#[derive(Clone)]
pub(crate) struct FlagType<T> {
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

    pub(crate) fn parse(&self, name: &str, value: Option<&[String]>) -> Result<T, FailedToParse> {
        (self.parse)(name, value)
    }

    pub(crate) fn arg_count(&self) -> FlagArgCount {
        self.arg
    }
}

pub(crate) fn string() -> FlagType<Option<String>> {
    FlagType::new(FlagArgCount::Arg, |_name, value| {
        Ok(match value {
            Some([x]) => Some(x.to_string()),
            _ => None,
        })
    })
}

pub(crate) fn bool_flag() -> FlagType<Option<bool>> {
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

pub(crate) fn enum_flag<T: Clone + Send + Sync + 'static>(
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

pub(crate) fn command_flag<T: Clone + Send + Sync + 'static>(
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

pub(crate) fn truthy() -> FlagType<bool> {
    FlagType::new(FlagArgCount::Truthy, |_name, value| Ok(value.is_some()))
}

pub(crate) fn required<T: Clone + Send + Sync + 'static>(
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

pub(crate) fn optional<T>(arg_type: FlagType<Option<T>>) -> FlagType<Option<T>> {
    arg_type
}

pub(crate) fn list_of<T: Send + Sync + 'static>(arg_type: FlagType<Option<T>>) -> FlagType<Vec<T>> {
    FlagType::new(FlagArgCount::ArgList, move |name, value| match value {
        None => Ok(vec![]),
        Some(values) => values
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
            .collect(),
    })
}

pub(crate) struct Spec {
    pub name: String,
    pub doc: String,
    pub usage: String,
    pub flags: BTreeMap<String, FlagMetadata>,
    pub anons: Vec<(String, FlagArgCount)>,
}

impl Spec {
    pub(crate) fn new(name: &str, doc: &str, usage: String) -> Self {
        let mut flags = BTreeMap::new();
        flags.insert(
            "--help".to_string(),
            FlagMetadata {
                doc: "This list of options".to_string(),
                env: None,
                arg_count: FlagArgCount::Truthy,
            },
        );
        Self {
            name: name.to_string(),
            doc: doc.to_string(),
            usage,
            flags,
            anons: vec![],
        }
    }

    pub(crate) fn flag<T>(
        mut self,
        name: &str,
        arg_type: &FlagType<T>,
        doc: &str,
        env: Option<&str>,
    ) -> Self {
        self.flags.insert(
            name.to_string(),
            FlagMetadata {
                doc: doc.to_string(),
                env: env.map(|env| env.to_string()),
                arg_count: arg_type.arg_count(),
            },
        );
        self
    }

    pub(crate) fn anon<T>(mut self, name: &str, arg_type: &FlagType<T>) -> Self {
        self.anons.push((name.to_string(), arg_type.arg_count()));
        self
    }
}

pub(crate) struct Command {
    cmdname: String,
    cmddoc: String,
    flags: BTreeMap<String, FlagMetadata>,
    anons: Vec<(String, FlagArgCount)>,
    string_of_usage: Box<dyn Fn() -> String + Send + Sync>,
    main: Box<dyn Fn(&Values) + Send + Sync>,
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

//     not !is_done) args
fn consume_args(args: &[String]) -> (Vec<String>, usize) {
    let mut consumed = Vec::new();
    let mut count = 0;
    while count < args.len() && !is_arg(&args[count]) {
        consumed.push(args[count].clone());
        count += 1;
    }
    (consumed, count)
}

//     by that name, then the flag without "no-" is looked up.
fn find_flag<'a>(
    name: &str,
    flags: &'a BTreeMap<String, FlagMetadata>,
) -> Result<(&'a str, &'a FlagMetadata), FailedToParse> {
    if let Some(flag) = flags.get_key_value(name) {
        return Ok((flag.0.as_str(), flag.1));
    }
    if let Some(name) = name.strip_prefix("--no-") {
        let name = format!("--{}", name);
        if let Some(flag) = flags.get_key_value(&name) {
            if flag.1.arg_count == FlagArgCount::NoArg {
                return Ok((flag.0.as_str(), flag.1));
            }
        }
    }
    Err(FailedToParse {
        arg: name.to_string(),
        msg: "Unknown option".to_string(),
        details: None,
    })
}

fn init_from_env(spec: &Spec) -> Values {
    let mut values = Values::new();
    for (arg, flag) in &spec.flags {
        if let Some(env) = &flag.env {
            match std::env::var(env) {
                Ok(value) if !value.is_empty() => {
                    values.insert(arg.clone(), vec![value]);
                }
                Ok(_) | Err(_) => {}
            }
        }
    }
    values
}

fn parse_argv(spec: &Spec, argv: &[String]) -> Result<Values, FailedToParse> {
    let mut values = init_from_env(spec);
    let mut anon_index = 0;
    let mut i = 0;
    while i < argv.len() {
        let arg = &argv[i];
        if is_arg(arg) {
            let (arg_name, inline_value) = match arg.split_once('=') {
                Some((arg, value)) => (arg.to_string(), Some(value.to_string())),
                None => (arg.clone(), None),
            };
            let (normalized_name, flag) = find_flag(&arg_name, &spec.flags)?;
            match flag.arg_count {
                FlagArgCount::Truthy => {
                    values.insert(normalized_name.to_string(), vec!["true".to_string()]);
                    i += 1;
                }
                FlagArgCount::NoArg => {
                    let value = if arg_name == normalized_name {
                        "true".to_string()
                    } else {
                        "false".to_string()
                    };
                    values.insert(normalized_name.to_string(), vec![value]);
                    i += 1;
                }
                FlagArgCount::Arg => {
                    let value = if let Some(value) = inline_value {
                        value
                    } else {
                        let value = argv.get(i + 1).ok_or_else(|| FailedToParse {
                            arg: arg_name.clone(),
                            msg: "Option needs an argument".to_string(),
                            details: None,
                        })?;
                        if is_arg(value) {
                            return Err(FailedToParse {
                                arg: arg_name.clone(),
                                msg: "Option needs an argument".to_string(),
                                details: None,
                            });
                        }
                        i += 1;
                        value.clone()
                    };
                    values.insert(normalized_name.to_string(), vec![value]);
                    i += 1;
                }
                FlagArgCount::ArgList => {
                    let mut value_list = Vec::new();
                    if let Some(value) = inline_value {
                        value_list.push(value);
                    }
                    let (rest, consumed) = consume_args(&argv[(i + 1)..]);
                    value_list.extend(rest);
                    values.insert(normalized_name.to_string(), value_list);
                    i += 1 + consumed;
                }
                FlagArgCount::ArgCommand => {
                    let mut command = inline_value.into_iter().collect::<Vec<_>>();
                    command.extend(argv[(i + 1)..].iter().cloned());
                    values.insert(normalized_name.to_string(), command);
                    break;
                }
            }
        } else {
            let (name, arg_count) =
                spec.anons
                    .get(anon_index)
                    .cloned()
                    .ok_or_else(|| FailedToParse {
                        arg: arg.clone(),
                        msg: "Unexpected argument".to_string(),
                        details: None,
                    })?;
            match arg_count {
                FlagArgCount::Arg => {
                    values.insert(name, vec![arg.clone()]);
                    anon_index += 1;
                    i += 1;
                }
                FlagArgCount::ArgList => {
                    let (value_list, consumed) = consume_args(&argv[i..]);
                    values.insert(name, value_list);
                    anon_index += 1;
                    i += consumed;
                }
                FlagArgCount::ArgCommand => {
                    values.insert(name, argv[i..].to_vec());
                    break;
                }
                FlagArgCount::Truthy | FlagArgCount::NoArg => {
                    unreachable!("anonymous truthy/no-arg values are not valid")
                }
            }
        }
    }
    Ok(values)
}

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
    if rows.is_empty() {
        spec.usage.clone()
    } else {
        format!(
            "{}\n{}",
            spec.usage,
            format_two_columns(None, None, 0, &rows)
        )
    }
}

pub(crate) fn command(spec: Spec, main: impl Fn(&Values) + Send + Sync + 'static) -> Command {
    let cmdname = spec.name.clone();
    let cmddoc = spec.doc.clone();
    let flags = spec.flags.clone();
    let anons = spec.anons.clone();
    let usage = usage_string(&spec);
    Command {
        cmdname,
        cmddoc,
        flags,
        anons,
        string_of_usage: Box::new(move || usage.clone()),
        main: Box::new(main),
    }
}

impl Command {
    pub(crate) fn run(&self, args: &Values) {
        (self.main)(args);
    }

    pub(crate) fn args_of_argv(&self, argv: &[String]) -> Result<Values, FailedToParse> {
        let spec = Spec {
            name: self.cmdname.clone(),
            doc: self.cmddoc.clone(),
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

pub(crate) fn get<T>(
    values: &Values,
    name: &str,
    arg_type: &FlagType<T>,
) -> Result<T, FailedToParse> {
    arg_type.parse(name, values.get(name).map(Vec::as_slice))
}

pub(crate) fn parse_or_show_help(
    command: &Command,
    argv: &[String],
) -> Result<Result<Values, ShowHelp>, FailedToParse> {
    let args = command.args_of_argv(argv)?;
    let help = get(&args, "--help", &truthy())?;
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
