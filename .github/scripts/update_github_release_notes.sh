#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -euo pipefail
set +x

REPO="${REPO:-${GITHUB_REPOSITORY:?GITHUB_REPOSITORY is required}}"
tag="${RELEASE_TAG:-${GITHUB_REF_NAME:-$(jq -r '.release.tag_name // empty' "$GITHUB_EVENT_PATH")}}"
export GH_TOKEN="${GH_TOKEN:-${GITHUB_TOKEN:-${FLOW_BOT_TOKEN:-}}}"

if [[ -z "$GH_TOKEN" ]]; then
  echo "GH_TOKEN or GITHUB_TOKEN is required to update release notes" >&2
  exit 1
fi

if [[ ! "$tag" =~ ^v[0-9]+\.[0-9]+\.[0-9]+([-+][0-9A-Za-z.-]+)?$ ]]; then
  echo "Skipping non-release tag: $tag"
  exit 0
fi

version="${tag#v}"
version_file="rust_port/crates/flow_common/src/flow_version.rs"
flow_version="$(sed -n 's/^const RELEASE_VERSION: &str = "\([^"]*\)";/\1/p' "$version_file")"
if [[ "$flow_version" != "$version" ]]; then
  echo "$version_file contains $flow_version, but the version bump commit is for $version" >&2
  exit 1
fi

changelog_version="$(awk '/^### / { sub(/^v/, "", $2); print $2; exit }' Changelog.md)"
if [[ "$changelog_version" != "$version" ]]; then
  echo "Changelog.md starts with $changelog_version, but the version bump commit is for $version" >&2
  exit 1
fi

notes_file="$(mktemp)"
trap 'rm -f "$notes_file"' EXIT
awk '/^### / { if (seen++) exit; next } seen' Changelog.md | sed -e '/./,$!d' > "$notes_file"

gh release edit "$tag" --repo "$REPO" --notes-file "$notes_file"
echo "Updated release notes for $tag"
