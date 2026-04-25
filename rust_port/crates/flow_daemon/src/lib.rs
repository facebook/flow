/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub mod daemon;
pub mod pid_log;

#[cfg(test)]
mod cross_process_tests;

pub use daemon::ChannelMode;
pub use daemon::ChannelPair;
pub use daemon::Entry;
pub use daemon::Handle;
pub use daemon::InChannel;
pub use daemon::OutChannel;
pub use daemon::StdioFd;
pub use daemon::check_entry_point;
pub use daemon::close;
pub use daemon::close_noerr;
pub use daemon::descr_of_in_channel;
pub use daemon::descr_of_out_channel;
pub use daemon::fd_of_path;
pub use daemon::flush;
pub use daemon::from_channel;
pub use daemon::into_in_reader;
pub use daemon::into_in_stream;
pub use daemon::into_out_stream;
pub use daemon::into_out_writer;
pub use daemon::kill;
pub use daemon::name_of_entry;
pub use daemon::null_fd;
pub use daemon::register_entry_point;
pub use daemon::shutdown_out_write;
pub use daemon::spawn;
pub use daemon::to_channel;
pub use daemon::try_from_channel;
