/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::VecDeque;
use std::fs;
use std::path::Path;

// Sys.readdir only returns `string list`, but we need to know if we have files
// or directories, so if we use Sys.readdir we need to do an lstat on every
// file/subdirectory. The C readdir function gives us both the name and kind,
// so this version does 1 syscall per directory, instead of 1 syscall per file
// and 2 per directory.
#[derive(Clone, Copy)]
enum DtKind {
    Reg,
    Dir,
}

fn lstat_kind(file: &str) -> Option<fs::FileType> {
    match fs::symlink_metadata(file) {
        Ok(meta) => Some(meta.file_type()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            eprintln!("File not found: {}", file);
            None
        }
        Err(_) => None,
    }
}

fn hh_readdir(path: &str) -> Vec<(String, DtKind)> {
    let entries = match fs::read_dir(path) {
        Ok(entries) => entries,
        Err(_) => return Vec::new(),
    };
    entries
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let name = entry.file_name().to_string_lossy().into_owned();
            match name.as_str() {
                ".git" | ".hg" => return None,
                _ => {}
            }
            let file_type = match entry.file_type() {
                Ok(ft) => ft,
                Err(_) => match lstat_kind(&Path::new(path).join(&name).to_string_lossy()) {
                    Some(ft) => ft,
                    None => return None,
                },
            };
            // DT_UNKNOWN - filesystem does not give us the type; do it slow
            if file_type.is_dir() {
                Some((name, DtKind::Dir))
            } else if file_type.is_file() {
                Some((name, DtKind::Reg))
            } else {
                None
            }
        })
        .collect()
}

enum Stack {
    Nil,
    Dir(VecDeque<(String, DtKind)>, String, Box<Stack>),
}

const MAX_FILES: usize = 1000;

pub fn make_next_files(
    filter: Box<dyn Fn(&str) -> bool>,
    others: Vec<String>,
    root: String,
) -> impl FnMut() -> Vec<String> {
    fn process(
        sz: usize,
        mut acc: Vec<String>,
        mut files: VecDeque<(String, DtKind)>,
        dir: String,
        stack: Stack,
        filter: &dyn Fn(&str) -> bool,
    ) -> (Vec<String>, Stack) {
        if sz >= MAX_FILES {
            return (acc, Stack::Dir(files, dir, Box::new(stack)));
        }
        match files.pop_front() {
            None => process_stack(sz, acc, stack, filter),
            Some((name, kind)) => {
                let name = if dir.is_empty() {
                    name
                } else {
                    format!("{}/{}", dir, name)
                };
                match kind {
                    DtKind::Reg if filter(&name) => {
                        acc.push(name);
                        process(sz + 1, acc, files, dir, stack, filter)
                    }
                    DtKind::Dir => {
                        let dirfiles = hh_readdir(&name);
                        process(
                            sz,
                            acc,
                            VecDeque::from(dirfiles),
                            name,
                            Stack::Dir(files, dir, Box::new(stack)),
                            filter,
                        )
                    }
                    _ => process(sz, acc, files, dir, stack, filter),
                }
            }
        }
    }

    fn process_stack(
        sz: usize,
        acc: Vec<String>,
        stack: Stack,
        filter: &dyn Fn(&str) -> bool,
    ) -> (Vec<String>, Stack) {
        match stack {
            Stack::Nil => (acc, Stack::Nil),
            Stack::Dir(files, dir, stack) => process(sz, acc, files, dir, *stack, filter),
        }
    }

    let mut dirs_input = vec![root];
    dirs_input.extend(others);
    let dirs: VecDeque<(String, DtKind)> = dirs_input
        .into_iter()
        .filter_map(|path| {
            let ft = lstat_kind(&path)?;
            if ft.is_file() {
                Some((path, DtKind::Reg))
            } else if ft.is_dir() {
                Some((path, DtKind::Dir))
            } else {
                None
            }
        })
        .collect();
    let mut state = Stack::Dir(dirs, String::new(), Box::new(Stack::Nil));

    move || {
        let (res, st) = process_stack(
            0,
            Vec::new(),
            std::mem::replace(&mut state, Stack::Nil),
            &*filter,
        );
        state = st;
        res
    }
}
