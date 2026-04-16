/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum FileInput {
    FileName(String),
    FileContent(Option<String>, String),
}

impl FileInput {
    pub fn path_of_file_input(&self) -> Option<&str> {
        match self {
            FileInput::FileName(f) => Some(f),
            FileInput::FileContent(Some(f), _) => Some(f),
            _ => None,
        }
    }

    pub fn filename_of_file_input(&self) -> &str {
        match self {
            FileInput::FileName(f) => f,
            FileInput::FileContent(Some(f), _) => f,
            FileInput::FileContent(None, _) => "-",
        }
    }

    pub fn content_of_file_input_unsafe(&self) -> String {
        match self {
            FileInput::FileName(f) => std::fs::read_to_string(f)
                .expect("content_of_file_input_unsafe: failed to read file"),
            FileInput::FileContent(_, content) => content.clone(),
        }
    }

    pub fn content_of_file_input(&self) -> Result<String, String> {
        match self {
            FileInput::FileName(f) => std::fs::read_to_string(f).map_err(|e| format!("{}", e)),
            FileInput::FileContent(_, content) => Ok(content.clone()),
        }
    }
}
