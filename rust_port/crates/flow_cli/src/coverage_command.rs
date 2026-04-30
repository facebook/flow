/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::offset_utils;
use flow_server_env::server_prot;
use flow_utils_tty::ColorMode;
use flow_utils_tty::RawColor;
use flow_utils_tty::Style;
use serde_json::json;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// ***********************************************************************
// flow coverage command
// ***********************************************************************

fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    let spec = command_spec::Spec::new(
        "coverage",
        "Shows coverage information for a given file",
        command_spec::Visibility::Public,
        format!(
            "Usage: {exe_name} coverage [OPTION]... [FILE]\n\ne.g. {exe_name} coverage foo.js\nor   {exe_name} coverage < foo.js\n"
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_connect_and_json_flags(spec);
    let spec = command_utils::add_root_flag(spec);
    let spec = command_utils::add_strip_root_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_wait_for_recheck_flag(spec);
    let spec = spec.flag(
        "--color",
        &arg_spec::truthy(),
        "Print the file with colors showing which parts have unknown types (blue for 'empty' and red for 'any'). Cannot be used with --json or --pretty",
        None,
    )
    .flag(
        "--debug",
        &arg_spec::truthy(),
        "Print debugging info about each range in the file to stderr. Cannot be used with --json or --pretty",
        None,
    );
    let spec = command_utils::add_path_flag(spec);
    spec.flag(
        "--all",
        &arg_spec::truthy(),
        "Ignore absence of @flow pragma",
        None,
    )
    .anon("file", &arg_spec::optional(arg_spec::string()))
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CoverageKind {
    Checked,
    Any,
    Empty,
}

impl CoverageKind {
    fn to_bool(self) -> bool {
        match self {
            Self::Checked => true,
            Self::Any | Self::Empty => false,
        }
    }

    fn m_or(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::Any, _) | (_, Self::Any) => Self::Any,
            (Self::Empty, kind) | (kind, Self::Empty) => kind,
            (Self::Checked, Self::Checked) => Self::Checked,
        }
    }
}

fn handle_error(err: String, json: bool, pretty: bool) {
    if json {
        flow_hh_json::prerr_json_endline(pretty, &serde_json::json!({ "error": err }));
    } else {
        eprintln!("{}", err);
    }
}

fn accum_coverage(
    (checked, empty, total): (usize, usize, usize),
    (_, cov): &(flow_parser::loc::Loc, CoverageKind),
) -> (usize, usize, usize) {
    match cov {
        CoverageKind::Any => (checked, empty, total + 1),
        CoverageKind::Empty => (checked, empty + 1, total + 1),
        CoverageKind::Checked => (checked + 1, empty, total + 1),
    }
}

fn accum_coverage_locs(
    (checked, empty, uncovered): (
        Vec<flow_parser::loc::Loc>,
        Vec<flow_parser::loc::Loc>,
        Vec<flow_parser::loc::Loc>,
    ),
    (loc, cov): &(flow_parser::loc::Loc, CoverageKind),
) -> (
    Vec<flow_parser::loc::Loc>,
    Vec<flow_parser::loc::Loc>,
    Vec<flow_parser::loc::Loc>,
) {
    match cov {
        CoverageKind::Any => (checked, empty, {
            let mut uncovered = uncovered;
            uncovered.push(loc.clone());
            uncovered
        }),
        CoverageKind::Empty => (
            checked,
            {
                let mut empty = empty;
                empty.push(loc.clone());
                empty
            },
            {
                let mut uncovered = uncovered;
                uncovered.push(loc.clone());
                uncovered
            },
        ),
        CoverageKind::Checked => (
            {
                let mut checked = checked;
                checked.push(loc.clone());
                checked
            },
            empty,
            uncovered,
        ),
    }
}

fn colorize(
    content: &str,
    from_offset: i64,
    to_offset: i64,
    color: CoverageKind,
    accum: &mut Vec<(CoverageKind, String)>,
) -> i64 {
    if to_offset > from_offset {
        let from_offset = from_offset as usize;
        let to_offset = to_offset as usize;
        accum.push((color, content[from_offset..to_offset].to_string()));
        to_offset as i64
    } else {
        from_offset
    }
}

fn debug_range((loc, cov): &(flow_parser::loc::Loc, CoverageKind)) {
    eprintln!(
        "{}:{},{}:{}: {}",
        loc.start.line,
        loc.start.column,
        loc.end.line,
        loc.end.column,
        cov.to_bool(),
    );
}

fn colorize_file(
    content: &str,
    mut last_offset: i64,
    ranges: &[((i64, i64), CoverageKind)],
) -> Vec<(CoverageKind, String)> {
    let mut accum = Vec::new();
    for ((offset, end_offset), kind) in ranges {
        // catch up to the start of this range
        last_offset = colorize(
            content,
            last_offset,
            *offset,
            CoverageKind::Checked,
            &mut accum,
        );
        last_offset = colorize(content, last_offset, *end_offset, *kind, &mut accum);
    }
    colorize(
        content,
        last_offset,
        content.len() as i64,
        CoverageKind::Checked,
        &mut accum,
    );
    accum
}

fn split_overlapping_ranges(
    ranges: Vec<((i64, i64), CoverageKind)>,
) -> Vec<((i64, i64), CoverageKind)> {
    let mut ranges: std::collections::VecDeque<((i64, i64), CoverageKind)> = ranges.into();
    let mut accum: Vec<((i64, i64), CoverageKind)> = Vec::new();
    while ranges.len() > 1 {
        let (loc1, kind1) = ranges.pop_front().unwrap();
        let (loc2, kind2) = ranges.pop_front().unwrap();
        let ((loc1_start, loc1_end), (loc2_start, loc2_end)) = (loc1, loc2);
        if loc1_end < loc2_start {
            // range 1 is completely before range 2, so consume range 1
            accum.push((loc1, kind1));
            ranges.push_front((loc2, kind2));
        } else if loc1_start == loc2_start {
            // range 1 and 2 start at the same place, so consume range 1 and
            // create a new range for the remainder of range 2, if any
            if loc1_end != loc2_end {
                let tail_loc = (loc1_end + 1, loc2_end);
                ranges.push_front((tail_loc, kind2));
                ranges.push_front((loc1, kind1));
                ranges
                    .make_contiguous()
                    .sort_by(|((a_line, a_col), _), ((b_line, b_col), _)| {
                        a_line.cmp(b_line).then(a_col.cmp(b_col))
                    });
            } else {
                ranges.push_front((loc1, kind1));
            }
        } else if loc1_end == loc2_end {
            // range 1 and 2 end at the same place, so split range 1 and consume
            // the first part, which doesn't overlap
            let head_loc = (loc1_start, loc2_start - 1);
            accum.push((head_loc, kind1));
            ranges.push_front((loc2, kind2));
        } else if loc1_end < loc2_end {
            // TODO: Given that at this point we also have loc1.start.offset <
            // loc2.start.offset, it means that range 1 and 2 overlap but don't
            // nest. Ideally, this case should never arise: we should be able to
            // guarantee the invariant that ranges (same as "spans" in
            // common/span.ml) are either disjoint or nest. However, some
            // combination of bugs and incompleteness in statement.ml and
            // parser_flow.ml cause this invariant to be violated. So here we are.
            //
            // Split range1, range2, and the overlapping part. Consume the first
            // part of range1, which doesn't overlap. Also consume the overlapping
            // part, which we assume to be small enough (usually, 1 token) to not
            // contain any interesting nested stuff (recall that the overlap is a
            // bug, not a feature), and optimistically consider it covered if
            // range1 or range2 is covered (because the alternative is 1-token
            // islands of uncovered stuff).
            let head_loc = (loc1_start, loc2_start - 1);
            let overlap_loc = (loc2_start, loc1_end);
            let tail_loc = (loc1_end + 1, loc2_end);
            accum.push((head_loc, kind1));
            accum.push((overlap_loc, CoverageKind::m_or(kind1, kind2)));
            ranges.push_front((tail_loc, kind2));
        } else {
            // range 2 is in the middle of range 1, so split range 1 and consume
            // the first part, which doesn't overlap, and then recurse on
            // range2::range1tail::rest
            let head_loc = (loc1_start, loc2_start - 1);
            let tail_loc = (loc2_end + 1, loc1_end);
            accum.push((head_loc, kind1));
            ranges.push_front((tail_loc, kind1));
            ranges.push_front((loc2, kind2));
            ranges
                .make_contiguous()
                .sort_by(|((a_line, a_col), _), ((b_line, b_col), _)| {
                    a_line.cmp(b_line).then(a_col.cmp(b_col))
                });
        }
    }
    accum.extend(ranges);
    accum
}

fn handle_response(
    json: bool,
    pretty: bool,
    strip_root: Option<&str>,
    color: bool,
    debug: bool,
    types: Vec<(flow_parser::loc::Loc, CoverageKind)>,
    content: String,
) {
    if debug {
        for item in &types {
            debug_range(item);
        }
    }

    let offset_table = if json || color {
        Some(offset_utils::OffsetTable::make(&content))
    } else {
        None
    };

    if color {
        let coverage_offsets = types
            .iter()
            .map(|(loc, covered)| {
                let offset_table = offset_table.as_ref().unwrap();
                // Rust-port-specific: convert byte->codepoint columns since the
                // OffsetTable is codepoint-indexed. See `flow_common::reason::json_of_loc_props`
                // for a fuller explanation.
                let start_pos = offset_table
                    .convert_flow_position_to_js_position(loc.start)
                    .unwrap();
                let end_pos = offset_table
                    .convert_flow_position_to_js_position(loc.end)
                    .unwrap();
                let start = offset_table.offset(start_pos).unwrap() as i64;
                let end = offset_table.offset(end_pos).unwrap() as i64;
                ((start, end), *covered)
            })
            .collect::<Vec<_>>();
        let coverage_offsets = split_overlapping_ranges(coverage_offsets);
        let colors = colorize_file(&content, 0, &coverage_offsets);
        let mut stdout = std::io::stdout();
        let styles = colors
            .iter()
            .map(|(color, text)| {
                let style = match color {
                    CoverageKind::Checked => Style::Normal(RawColor::Default),
                    CoverageKind::Any => Style::Normal(RawColor::Red),
                    CoverageKind::Empty => Style::Normal(RawColor::Blue),
                };
                (style, text.clone())
            })
            .collect::<Vec<_>>();
        flow_utils_tty::cprint(&mut stdout, &styles, Some(ColorMode::ColorAlways)).unwrap();
        println!();
    }

    let (covered, empty, total) = types.iter().fold((0, 0, 0), accum_coverage);
    let percent = if total == 0 {
        100.0
    } else {
        covered as f64 / total as f64 * 100.0
    };

    if json {
        let (mut covered_locs, empty_locs, uncovered_locs) = types
            .iter()
            .fold((Vec::new(), Vec::new(), Vec::new()), accum_coverage_locs);
        covered_locs.sort();
        let offset_table = offset_table.as_ref();
        let json_output = json!({
            "expressions": {
                "covered_count": covered,
                "covered_locs": covered_locs
                    .iter()
                    .map(|loc| flow_common::reason::json_of_loc(strip_root, false, offset_table, loc))
                    .collect::<Vec<_>>(),
                "uncovered_count": total - covered,
                "uncovered_locs": uncovered_locs
                    .iter()
                    .map(|loc| flow_common::reason::json_of_loc(strip_root, false, offset_table, loc))
                    .collect::<Vec<_>>(),
                "empty_count": empty,
                "empty_locs": empty_locs
                    .iter()
                    .map(|loc| flow_common::reason::json_of_loc(strip_root, false, offset_table, loc))
                    .collect::<Vec<_>>(),
            },
        });
        flow_hh_json::print_json_endline(pretty, &json_output);
    } else {
        println!(
            "Covered: {:.2}% ({} of {} expressions)\n",
            percent, covered, total
        );
    }
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let connect_flags = command_utils::get_connect_flags(args);
    let json_flags = command_utils::get_json_flags(args);
    let root_arg =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let wait_for_recheck = command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let color = command_spec::get(args, "--color", &arg_spec::truthy()).unwrap();
    let debug = command_spec::get(args, "--debug", &arg_spec::truthy()).unwrap();
    let path = command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
    let force = command_spec::get(args, "--all", &arg_spec::truthy()).unwrap();
    let filename =
        command_spec::get(args, "file", &arg_spec::optional(arg_spec::string())).unwrap();

    // pretty implies json
    let json = json_flags.json || json_flags.pretty;
    if color && json {
        eprintln!("--color can't be used with --json or --pretty");
        flow_common_exit_status::exit(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
        );
    }
    if debug && json {
        eprintln!("--debug can't be used with --json or --pretty");
        flow_common_exit_status::exit(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
        );
    }

    let file = command_utils::get_file_from_filename_or_stdin(
        "coverage",
        filename.as_deref(),
        path.as_deref(),
    );
    let root = command_utils::guess_root(
        &flowconfig_name,
        match root_arg.as_deref() {
            Some(root) => Some(root),
            None => file.path_of_file_input(),
        },
    );
    let strip_root = if strip_root {
        Some(root.to_string_lossy().to_string())
    } else {
        None
    };
    let request = server_prot::request::Command::COVERAGE {
        input: file.clone(),
        force,
        wait_for_recheck,
    };
    let response =
        command_utils::connect_and_make_request(&flowconfig_name, &connect_flags, &root, &request);
    match response {
        server_prot::response::Response::COVERAGE(Err(err)) => {
            handle_error(err, json, json_flags.pretty);
        }
        server_prot::response::Response::COVERAGE(Ok(response)) => {
            let content = file.content_of_file_input_unsafe();
            let response = response
                .into_iter()
                .map(|(loc, cov)| {
                    let cov = match cov.to_str() {
                        "Checked" => CoverageKind::Checked,
                        "Any" => CoverageKind::Any,
                        "Empty" => CoverageKind::Empty,
                        other => panic!("Unexpected coverage kind: {}", other),
                    };
                    (loc, cov)
                })
                .collect();
            handle_response(
                json,
                json_flags.pretty,
                strip_root.as_deref(),
                color,
                debug,
                response,
                content,
            );
        }
        response => {
            command_utils::failwith_bad_response(&request, &response);
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
