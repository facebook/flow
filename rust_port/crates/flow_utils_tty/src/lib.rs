/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! TTY utilities for terminal output with colors and styles.
//!
//! This is a port of hack_forked/utils/sys/tty.ml

use std::io::IsTerminal;
use std::io::Write;
use std::process::Command;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

/// Raw terminal colors
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RawColor {
    Default,
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
}

/// Terminal text styles
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Style {
    Normal(RawColor),
    Bold(RawColor),
    Dim(RawColor),
    Italics(RawColor),
    Underline(RawColor),
    BoldDim(RawColor),
    BoldItalics(RawColor),
    BoldUnderline(RawColor),
    DimUnderline(RawColor),
    NormalWithBG(RawColor, RawColor),
    BoldWithBG(RawColor, RawColor),
}

/// Color mode for output
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorMode {
    ColorAlways,
    ColorNever,
    ColorAuto,
}

impl RawColor {
    /// Get ANSI color code for foreground
    fn fg_code(&self) -> &'static str {
        match self {
            RawColor::Default => "39",
            RawColor::Black => "30",
            RawColor::Red => "31",
            RawColor::Green => "32",
            RawColor::Yellow => "33",
            RawColor::Blue => "34",
            RawColor::Magenta => "35",
            RawColor::Cyan => "36",
            RawColor::White => "37",
        }
    }

    /// Get ANSI style color code matching tty.ml's `color_num`
    fn style_code(&self) -> &'static str {
        match self {
            RawColor::Default => "0",
            color => color.fg_code(),
        }
    }

    /// Get ANSI color code for background
    fn bg_code(&self) -> &'static str {
        match self {
            RawColor::Default => "49",
            RawColor::Black => "40",
            RawColor::Red => "41",
            RawColor::Green => "42",
            RawColor::Yellow => "43",
            RawColor::Blue => "44",
            RawColor::Magenta => "45",
            RawColor::Cyan => "46",
            RawColor::White => "47",
        }
    }
}

/// Check if we should use colors based on mode
pub fn should_color(mode: ColorMode) -> bool {
    match mode {
        ColorMode::ColorAlways => true,
        ColorMode::ColorNever => false,
        ColorMode::ColorAuto => supports_color(),
    }
}

/// Check if the terminal supports color
pub fn supports_color() -> bool {
    match std::env::var("TERM") {
        Ok(term) => std::io::stdout().is_terminal() && term != "dumb",
        Err(_) => false,
    }
}

/// Apply color styling to a string
pub fn apply_color(style: Style, text: &str, color_mode: Option<ColorMode>) -> String {
    let mode = color_mode.unwrap_or(ColorMode::ColorAuto);
    if !should_color(mode) {
        return text.to_string();
    }

    let (codes, reset) = match style {
        Style::Normal(c) => (format!("\x1b[{}m", c.style_code()), "\x1b[0m"),
        Style::Bold(c) => (format!("\x1b[{};1m", c.style_code()), "\x1b[0m"),
        Style::Dim(c) => (format!("\x1b[{};2m", c.style_code()), "\x1b[0m"),
        Style::Italics(c) => (format!("\x1b[{};3m", c.style_code()), "\x1b[0m"),
        Style::Underline(c) => (format!("\x1b[{};4m", c.style_code()), "\x1b[0m"),
        Style::BoldDim(c) => (format!("\x1b[{};1;2m", c.style_code()), "\x1b[0m"),
        Style::BoldItalics(c) => (format!("\x1b[{};1;3m", c.style_code()), "\x1b[0m"),
        Style::BoldUnderline(c) => (format!("\x1b[{};1;4m", c.style_code()), "\x1b[0m"),
        Style::DimUnderline(c) => (format!("\x1b[{};2;4m", c.style_code()), "\x1b[0m"),
        Style::NormalWithBG(fg, bg) => (
            format!("\x1b[{};{}m", fg.fg_code(), bg.bg_code()),
            "\x1b[0m",
        ),
        Style::BoldWithBG(fg, bg) => (
            format!("\x1b[1;{};{}m", fg.fg_code(), bg.bg_code()),
            "\x1b[0m",
        ),
    };

    format!("{}{}{}", codes, text, reset)
}

/// Print colorized strings to a writer
pub fn cprint<W: Write>(
    writer: &mut W,
    styles: &[(Style, String)],
    color_mode: Option<ColorMode>,
) -> std::io::Result<()> {
    for (style, text) in styles {
        write!(writer, "{}", apply_color(*style, text, color_mode))?;
    }
    Ok(())
}

static SPINNER_STATE: AtomicUsize = AtomicUsize::new(0);
const ASCII_SPINNER: [&str; 4] = ["-", "\\", "|", "/"];

// Some terminals display the emoji using only one column, even though they
// may take up two columns, and put the cursor immediately after it in an
// illegible manner. Add an extra space to separate the cursor from the emoji.
const EMOJI_SPINNER: [&str; 4] = ["\u{1F621} ", "\u{1F602} ", "\u{1F914} ", "\u{1F4AF} "];

/// Return the next string in the four-state spinner.
pub fn spinner(angery_reaccs_only: bool) -> &'static str {
    let spinner = if angery_reaccs_only {
        &EMOJI_SPINNER
    } else {
        &ASCII_SPINNER
    };
    let state = SPINNER_STATE.fetch_add(1, Ordering::SeqCst);
    spinner[state % 4]
}

pub fn spinner_used() -> bool {
    SPINNER_STATE.load(Ordering::SeqCst) != 0
}

/// ANSI escape sequence to clear whole line  
const CLEAR_LINE_SEQ: &str = "\r\x1b[0K";

/// Output a "clear current line" escape sequence if the writer is a TTY.
pub fn print_clear_line<W: Write + IsTerminal>(writer: &mut W) -> std::io::Result<()> {
    if writer.is_terminal() {
        write!(writer, "{}", CLEAR_LINE_SEQ)?;
        writer.flush()?;
    }
    Ok(())
}

// See https://github.com/yarnpkg/yarn/issues/405.
pub fn supports_emoji() -> bool {
    // On non-Windows (always true on Linux), check if color is supported
    cfg!(not(target_os = "windows")) && supports_color()
}

/// Gets the column width of the current terminal.
pub fn get_term_cols() -> Option<i32> {
    if !cfg!(unix) || !supports_color() {
        return None;
    }
    Command::new("tput")
        .arg("cols")
        .output()
        .ok()
        .and_then(|output| {
            String::from_utf8(output.stdout)
                .ok()
                .and_then(|s| s.trim().parse::<i32>().ok())
        })
}
