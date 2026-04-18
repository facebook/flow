/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::flow_import_specifier::Userland;
use flow_common_modulename::ModulenameSet;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_heap::parsing_heaps::SharedMem;
use flow_heap::resolved_requires::Dependency;
use flow_heap::resolved_requires::ResolvedModule;
use flow_parser::file_key::FileKey;
use flow_server_env::DependencyInfo;
use flow_server_env::PartialDependencyGraph;
use flow_utils_concurrency::map_reduce;
use flow_utils_concurrency::thread_pool::ThreadPool;

// sketch of baseline incremental alg:
//
// Note: this is for strict mode only, which is the easy case,
// since it doesn't require a transitive merge of all modules
// connected via nonlocal type flows.
//
// Total case over set of modules M:
//  1. for all m:M, do local inference on m, producing graph m.g
//  2. for all m:M, for all r:m.required,
//    perform substitution of r.export into m.g, producing m.g'
//
// Incremental case, given previously established modules m:
//  1. Mt = (touched) reparse of all m:M where m.file has been updated
//  2. for all m:Mt do local inference on m, producing m.g
//  3. Mx = (export) m:Mt where m.export or m.name have changed
//  4. Md = m:M where m.file has been deleted
//  5. Mr = (recheck) m:Mt | (M where !empty(m.required & (Mx | Md))
//    (all touched modules plus all others whose required sets
//     include any export changes or deletions)
//  5. for all m:Mr, for all r:m.required,
//      perform substitution of r.export into m.g, producing m.g'

// A note on terminology.
//
// We say that a file F1 "depends on" another file F2 if F1 has a
// require/import that "resolves" to a module "provided by" F2.
//
// The "depends on" relation induces edges in a "dependency graph" with files
// as nodes. This dependency graph is often represented as a map from files to
// sets of files they depend on.
//
// When F1 depends on F2, i.e., there is an edge from F1 to F2 in
// the dependency graph, we say that
// - F2 is a "direct dependency" of F1
// - F1 is a "direct dependent" of F2.
//
// When F1 transitively depends on F2, i.e., there is a path from F1 to F2 in
// the dependency graph, we say that
// - F2 is in "all dependencies" of F1
// - F1 is in "all dependents" of F2.
//
// Sometimes we drop the word "all," so in general "dependencies" and
// "dependents" on their own should be taken to mean "all dependencies" and
// "all dependents." Correspondingly, sometimes we use the qualifier "direct"
// to describe non-transitive dependencies and non-transitive dependents.
//
// Also, sometimes we use the word "reverse dependency" instead of "dependent."
//
// How do we calculate the dependency graph and the dependent graph? It comes
// down to understanding what "depends on" really means. Recall that F1
// "depends on" F2 when F1 depends on some resolved module that is provided by
// F2. Since the "provides by" function is not injective, maps from files to
// (sets of) modules and back that compose to give the dependency graph and the
// dependent graph are useful intermediate data structures.

// Identify the direct dependents of new, changed, and deleted files.
//
// Files that must be rechecked include those that immediately or recursively
// depended on modules whose providers were affected by new, changed, or deleted
// files. The latter modules, marked "changed," are calculated earlier when
// picking providers.
//
// - candidates is the set of files which could be dependents. The returned sets will be subsets of
//   the candidates set. For example, if we're calculating the dependents of all the changed files
//   then this would be the set of unchanged files
// - root_files is the set of files for which we'd like to calculate dependents. This should be
//   disjoint from candidates. If we wanted to calculate the dependents of all the changed files then
//   this would be the set of changed files
// - root_modules is the set of modules for which we'd like to calculate dependents. If we wanted to
//   calculate the dependents of all the changed files then this would be the set of module names
//   which have new providers.
//
// Return the subset of candidates directly dependent on root_modules / root_files.
pub fn calc_unchanged_dependents(
    shared_mem: &SharedMem,
    _workers: Option<Vec<()>>,
    changed_modules: ModulenameSet,
) -> BTreeSet<FileKey> {
    let mut acc = BTreeSet::new();

    let mut iter_f = |file: &FileKey| {
        // Skip dependents which have themselves changed, since changed files will
        // already be part of the recheck set.
        let file_changed = || -> bool { shared_mem.file_has_changed(file) };

        if !file_changed() {
            acc.insert(file.dupe());
        }
    };

    for m in changed_modules.iter() {
        shared_mem.iter_dependents(&mut iter_f, m);
    }

    acc
}

// Calculate module dependencies. Since this involves a lot of reading from
// shared memory, it is useful to parallelize this process (leading to big
// savings in init and recheck times).

// A file is considered to implement a required module r only if the file is
// registered to provide r and the file is checked. Such a file must be merged
// before any file that requires module r, so this notion naturally gives rise
// to a dependency ordering among files for merging.
fn implementation_file(
    shared_mem: &SharedMem,
    resolved_module: &ResolvedModule,
) -> Option<FileKey> {
    match resolved_module {
        ResolvedModule::HasteModule(modulename) => {
            let dependency = Dependency::HasteModule(modulename.clone());
            if let Some(f) = shared_mem.get_provider(&dependency) {
                if shared_mem.is_typed_file(&f) {
                    return Some(f);
                }
            }
            None
        }
        ResolvedModule::File(f) => {
            let dependency = Dependency::File(f.dupe());
            if let Some(f) = shared_mem.get_provider(&dependency) {
                if shared_mem.is_typed_file(&f) {
                    return Some(f);
                }
            }
            None
        }
        ResolvedModule::String(_) | ResolvedModule::Null => None,
    }
}

fn file_dependencies(
    shared_mem: &SharedMem,
    file: &FileKey,
    parsed: &FlowOrdSet<FileKey>,
) -> (BTreeSet<FileKey>, BTreeSet<FileKey>) {
    let type_sig = shared_mem.get_type_sig_unsafe(file);
    let sig_require_set: HashSet<&Userland> = type_sig.module_refs.iter().collect();

    let requires = shared_mem.get_requires_unsafe(file);
    let resolved_requires = shared_mem.get_resolved_requires_unsafe(file);
    let resolved_modules = resolved_requires.get_resolved_modules();

    let mut sig_files = BTreeSet::new();
    let mut all_files = BTreeSet::new();

    for (mref, resolved_module) in requires.iter().zip(resolved_modules.iter()) {
        if let Some(f) = implementation_file(shared_mem, resolved_module) {
            if parsed.contains(&f) {
                all_files.insert(f.dupe());
                if let FlowImportSpecifier::Userland(u) = mref {
                    if sig_require_set.contains(u) {
                        sig_files.insert(f);
                    }
                }
            }
        }
    }

    (sig_files, all_files)
}

// Calculates the dependency graph as a map from files to their dependencies.
// Dependencies not in parsed are ignored.
pub fn calc_partial_dependency_graph(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    files: &FlowOrdSet<FileKey>,
    parsed: &FlowOrdSet<FileKey>,
) -> PartialDependencyGraph {
    let files_vec: Vec<FileKey> = files.iter().map(|f| f.dupe()).collect();
    let next = map_reduce::make_next(
        pool.num_workers(),
        None::<fn(i32, i32, i32)>,
        None,
        files_vec,
    );

    let dependency_graph: BTreeMap<FileKey, (BTreeSet<FileKey>, BTreeSet<FileKey>)> =
        map_reduce::call(
            pool,
            next,
            |acc: &mut BTreeMap<FileKey, (BTreeSet<FileKey>, BTreeSet<FileKey>)>,
             batch: Vec<FileKey>| {
                for file in batch {
                    let dependencies = file_dependencies(shared_mem, &file, parsed);
                    acc.insert(file, dependencies);
                }
            },
            |a, b| {
                a.extend(b);
            },
        );

    PartialDependencyGraph::from_map(dependency_graph)
}

pub fn calc_dependency_info(
    pool: &ThreadPool,
    shared_mem: &Arc<SharedMem>,
    parsed: &FlowOrdSet<FileKey>,
) -> DependencyInfo {
    let dependency_graph = calc_partial_dependency_graph(pool, shared_mem, parsed, parsed);
    DependencyInfo::of_map(pool, dependency_graph)
}
