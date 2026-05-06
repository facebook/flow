/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering as AtomicOrdering;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

#[derive(Clone, Copy, Debug)]
struct FloatKey(f64);

impl PartialEq for FloatKey {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for FloatKey {}

impl PartialOrd for FloatKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FloatKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}

#[derive(Clone)]
struct Distribution {
    bucket_size: f64,
    buckets: BTreeMap<FloatKey, f64>,
}

#[derive(Clone)]
struct RecordEntry {
    count: f64,
    mean: f64,
    variance_sum: f64,
    max: f64,
    min: f64,
    distribution: Option<Distribution>,
}

#[derive(Clone, Default)]
pub struct RecordData(BTreeMap<String, RecordEntry>);

#[derive(Clone)]
pub struct Record(Arc<Mutex<RecordData>>);

// Creates a new empty record
pub fn create() -> Record {
    Record(Arc::new(Mutex::new(RecordData::default())))
}

static GLOBAL: LazyLock<Mutex<Vec<Record>>> = LazyLock::new(|| Mutex::new(vec![create()]));

static ENABLED: AtomicBool = AtomicBool::new(false);

pub fn is_enabled() -> bool {
    ENABLED.load(AtomicOrdering::Relaxed)
}

pub fn set_enabled(enabled: bool) {
    ENABLED.store(enabled, AtomicOrdering::Relaxed);
}

pub fn push_global() {
    let mut global = GLOBAL
        .lock()
        .expect("Measure global stack lock should not be poisoned");
    global.insert(0, create());
}

pub fn pop_global() -> Record {
    let mut global = GLOBAL
        .lock()
        .expect("Measure global stack lock should not be poisoned");
    if global.is_empty() {
        panic!("Measure.pop_global called with empty stack");
    }
    global.remove(0)
}

pub fn serialize(record: &Record) -> RecordData {
    record
        .0
        .lock()
        .expect("Measure record lock should not be poisoned")
        .clone()
}

pub fn deserialize(data: RecordData) -> Record {
    Record(Arc::new(Mutex::new(data)))
}

fn new_entry() -> RecordEntry {
    RecordEntry {
        count: 0.0,
        mean: 0.0,
        variance_sum: 0.0,
        max: f64::MIN_POSITIVE,
        min: f64::MAX,
        distribution: None,
    }
}

fn new_distribution(bucket_size: f64) -> Option<Distribution> {
    Some(Distribution {
        bucket_size,
        buckets: BTreeMap::new(),
    })
}

fn get_record(record: Option<&Record>) -> Record {
    match record {
        Some(record) => record.clone(),
        None => {
            let global = GLOBAL
                .lock()
                .expect("Measure global stack lock should not be poisoned");
            match global.first() {
                Some(record) => record.clone(),
                None => panic!(
                    "{}{}",
                    "No global record available! ", "Did you forget to call Measure.push_global?"
                ),
            }
        }
    }
}

// Measure can track how the values are distributed by creating buckets and
// keeping track of how many samples fall into each buckets. It will not track
// distribution by default, so call this function to turn it on
pub fn track_distribution(record: Option<&Record>, name: &str, bucket_size: f64) {
    let record = get_record(record);
    let mut record = record
        .0
        .lock()
        .expect("Measure record lock should not be poisoned");
    let mut entry = record.0.get(name).cloned().unwrap_or_else(new_entry);
    entry.distribution = new_distribution(bucket_size);
    record.0.insert(name.to_string(), entry);
}

fn round_down(bucket_size: f64, value: f64) -> f64 {
    bucket_size * (value / bucket_size).floor()
}

fn update_distribution(
    weight: f64,
    value: f64,
    distribution: Option<Distribution>,
) -> Option<Distribution> {
    match distribution {
        None => None,
        Some(Distribution {
            bucket_size,
            mut buckets,
        }) => {
            let bucket = round_down(bucket_size, value);
            let bucket_count = match buckets.get(&FloatKey(bucket)) {
                None => weight,
                Some(count) => count + weight,
            };
            buckets.insert(FloatKey(bucket), bucket_count);
            Some(Distribution {
                bucket_size,
                buckets,
            })
        }
    }
}

pub fn sample(record: Option<&Record>, weight: Option<f64>, name: &str, value: f64) {
    let weight = weight.unwrap_or(1.0);
    let record = get_record(record);
    let mut record = record
        .0
        .lock()
        .expect("Measure record lock should not be poisoned");
    let RecordEntry {
        count: old_count,
        mean: old_mean,
        variance_sum,
        max,
        min,
        distribution,
    } = record.0.get(name).cloned().unwrap_or_else(new_entry);
    // Add 1 * weight to the count
    let count = old_count + weight;
    let mean = old_mean + (weight * (value - old_mean) / count);
    // Knuth's online variance approximation algorithm, updated for weights.
    // Weighted version from http://people.ds.cam.ac.uk/fanf2/hermes/doc/antiforgery/stats.pdf
    let variance_sum = variance_sum + (weight * (value - old_mean) * (value - mean));
    let max = max.max(value);
    let min = min.min(value);
    let distribution = update_distribution(weight, value, distribution);
    let entry = RecordEntry {
        count,
        mean,
        variance_sum,
        max,
        min,
        distribution,
    };
    record.0.insert(name.to_string(), entry);
}

pub fn delete(record: Option<&Record>, name: &str) {
    let record = get_record(record);
    record
        .0
        .lock()
        .expect("Measure record lock should not be poisoned")
        .0
        .remove(name);
}

fn merge_entries(
    name: &str,
    from: Option<RecordEntry>,
    into: Option<RecordEntry>,
) -> Option<RecordEntry> {
    match (from, into) {
        (None, into) => into,
        (from, None) => from,
        (Some(from), into) if from.count.total_cmp(&0.0) == Ordering::Equal => into,
        (from, Some(into)) if into.count.total_cmp(&0.0) == Ordering::Equal => from,
        (Some(from), Some(into)) => {
            let count = from.count + into.count;
            // Using this algorithm to combine the variance sums
            // https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
            // d = meanB - meanA
            let delta = from.mean - into.mean;
            // mean = meanA + delta * (countB/count)
            let mean = into.mean + (delta * from.count / count);
            // VarSum = VarSumA + VarSumB + delta * delta * countA * countB / count
            let variance_sum = from.variance_sum
                + into.variance_sum
                + (delta * delta * into.count * from.count / count);
            let max = from.max.max(into.max);
            let min = from.min.min(into.min);
            let distribution = match (from.distribution, into.distribution) {
                (None, into) => into,
                (from, None) => from,
                (
                    Some(Distribution {
                        bucket_size: from, ..
                    }),
                    Some(Distribution {
                        bucket_size: into, ..
                    }),
                ) if from.total_cmp(&into) != Ordering::Equal => {
                    panic!(
                        "Merging buckets for {} failed: bucket sizes {}, {}",
                        name, from, into
                    )
                }
                (
                    Some(Distribution {
                        bucket_size,
                        buckets: from,
                    }),
                    Some(Distribution { buckets: into, .. }),
                ) => {
                    let mut buckets = into;
                    for (bucket, from_count) in from {
                        match buckets.get_mut(&bucket) {
                            None => {
                                buckets.insert(bucket, from_count);
                            }
                            Some(into_count) => {
                                *into_count += from_count;
                            }
                        }
                    }
                    Some(Distribution {
                        bucket_size,
                        buckets,
                    })
                }
            };
            Some(RecordEntry {
                count,
                mean,
                variance_sum,
                max,
                min,
                distribution,
            })
        }
    }
}

// Merges all the samples from "from" into "record". If "record" is omitted
// then it uses the global record
pub fn merge(record: Option<&Record>, from: &Record) {
    let into = get_record(record);
    let from_data = serialize(from);
    let mut into_data = into
        .0
        .lock()
        .expect("Measure record lock should not be poisoned");
    let old_into = into_data.clone();
    let mut keys = BTreeSet::new();
    keys.extend(from_data.0.keys().cloned());
    keys.extend(old_into.0.keys().cloned());
    for name in keys {
        match merge_entries(
            &name,
            from_data.0.get(&name).cloned(),
            old_into.0.get(&name).cloned(),
        ) {
            None => {
                into_data.0.remove(&name);
            }
            Some(entry) => {
                into_data.0.insert(name, entry);
            }
        }
    }
}

fn now_seconds() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64()
}

pub fn time<T>(record: Option<&Record>, name: &str, f: impl FnOnce() -> T) -> T {
    let record = get_record(record);
    let start_time = now_seconds();
    let ret = f();
    let end_time = now_seconds();
    sample(Some(&record), None, name, end_time - start_time);
    ret
}

fn get_helper(
    record: Option<&Record>,
    name: &str,
    f: impl FnOnce(&RecordEntry) -> f64,
) -> Option<f64> {
    let record = get_record(record);
    let record = record
        .0
        .lock()
        .expect("Measure record lock should not be poisoned");
    record.0.get(name).map(f)
}

pub fn get_sum(record: Option<&Record>, name: &str) -> Option<f64> {
    get_helper(record, name, |RecordEntry { count, mean, .. }| count * mean)
}

pub fn get_mean(record: Option<&Record>, name: &str) -> Option<f64> {
    get_helper(record, name, |RecordEntry { mean, .. }| *mean)
}

pub fn get_count(record: Option<&Record>, name: &str) -> Option<f64> {
    get_helper(record, name, |RecordEntry { count, .. }| *count)
}

pub fn get_max(record: Option<&Record>, name: &str) -> Option<f64> {
    get_helper(record, name, |RecordEntry { max, .. }| *max)
}

fn print_raw(print_raw: Option<&dyn Fn(&str)>, message: &str) {
    match print_raw {
        None => eprintln!("{}", message),
        Some(print_raw) => print_raw(message),
    }
}

fn pretty_num(f: f64) -> String {
    if f > 1_000_000_000.0 {
        format!("{:.3}G", f / 1_000_000_000.0)
    } else if f > 1_000_000.0 {
        format!("{:.3}M", f / 1_000_000.0)
    } else if f > 1_000.0 {
        format!("{:.3}K", f / 1_000.0)
    } else if f.total_cmp(&f.floor()) == Ordering::Equal {
        format!("{}", f as i64)
    } else {
        format!("{:.6}", f)
    }
}

pub fn print_entry_stats(record: Option<&Record>, print_raw_fn: Option<&dyn Fn(&str)>, name: &str) {
    let record = get_record(record);
    let prefix = format!("{} stats --", name);
    let entry = record
        .0
        .lock()
        .expect("Measure record lock should not be poisoned")
        .0
        .get(name)
        .cloned();
    match entry {
        None => print_raw(print_raw_fn, &format!("{} NO DATA", prefix)),
        Some(RecordEntry { count, .. }) if count.total_cmp(&0.0) == Ordering::Equal => {
            print_raw(print_raw_fn, &format!("{} NO DATA", prefix));
        }
        Some(RecordEntry {
            count,
            mean,
            variance_sum,
            max,
            min,
            distribution: _,
        }) => {
            let total = count * mean;
            let std_dev = (variance_sum / count).sqrt();
            print_raw(
                print_raw_fn,
                &format!(
                    "{} samples: {}, total: {}, avg: {}, stddev: {}, max: {}, min: {})",
                    prefix,
                    pretty_num(count),
                    pretty_num(total),
                    pretty_num(mean),
                    pretty_num(std_dev),
                    pretty_num(max),
                    pretty_num(min)
                ),
            );
        }
    }
}

pub fn print_stats(record: Option<&Record>, print_raw_fn: Option<&dyn Fn(&str)>) {
    let record = get_record(record);
    let names: Vec<String> = record
        .0
        .lock()
        .expect("Measure record lock should not be poisoned")
        .0
        .keys()
        .cloned()
        .collect();
    for name in names {
        print_entry_stats(Some(&record), print_raw_fn, &name);
    }
}

fn print_buckets(low: f64, high: f64, bucket_size: f64, buckets: &BTreeMap<FloatKey, f64>) {
    if low <= high {
        let count = match buckets.get(&FloatKey(low)) {
            None => 0.0,
            Some(count) => *count,
        };
        eprint!("[{}: {}]  ", pretty_num(low), pretty_num(count));
        let low = low + bucket_size;
        print_buckets(low, high, bucket_size, buckets);
    }
}

pub fn print_entry_distribution(record: Option<&Record>, name: &str) {
    let record = get_record(record);
    eprint!("{} distribution -- ", name);
    let entry = record
        .0
        .lock()
        .expect("Measure record lock should not be poisoned")
        .0
        .get(name)
        .cloned();
    match entry {
        None => eprintln!("NO DATA"),
        Some(RecordEntry { count, .. }) if count.total_cmp(&0.0) == Ordering::Equal => {
            eprintln!("NO DATA");
        }
        Some(RecordEntry {
            distribution: None, ..
        }) => eprintln!("NO DATA (did you forget to call track_distribution?)"),
        Some(RecordEntry {
            max,
            min,
            distribution:
                Some(Distribution {
                    bucket_size,
                    buckets,
                }),
            ..
        }) => {
            let low = round_down(bucket_size, min);
            let high = round_down(bucket_size, max);
            print_buckets(low, high, bucket_size, &buckets);
            eprintln!();
        }
    }
}

pub fn print_distributions(record: Option<&Record>) {
    let record = get_record(record);
    let names: Vec<String> = record
        .0
        .lock()
        .expect("Measure record lock should not be poisoned")
        .0
        .iter()
        .filter_map(|(name, entry)| {
            if entry.distribution.is_some() {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect();
    for name in names {
        print_entry_distribution(Some(&record), &name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn samples_weighted_stats() {
        let record = create();
        sample(Some(&record), None, "x", 1.0);
        sample(Some(&record), Some(2.0), "x", 4.0);
        assert_eq!(get_count(Some(&record), "x"), Some(3.0));
        assert_eq!(get_sum(Some(&record), "x"), Some(9.0));
        assert_eq!(get_max(Some(&record), "x"), Some(4.0));
    }

    #[test]
    fn merges_records() {
        let into = create();
        let from = create();
        sample(Some(&into), None, "x", 1.0);
        sample(Some(&from), None, "x", 3.0);
        merge(Some(&into), &from);
        assert_eq!(get_count(Some(&into), "x"), Some(2.0));
        assert_eq!(get_mean(Some(&into), "x"), Some(2.0));
    }
}
