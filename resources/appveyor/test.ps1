# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

pushd "C:\projects\flow"
try {
  echo "Running tool tests"
  echo "Using node version $(node --version)"
  node packages\flow-dev-tools\bin\tool test --parallelism 1 --max-errored-tests-pct 20 2>&1 | %{ "$_" }
  if ($LASTEXITCODE -gt 0) {
    Throw "node tool test exited with error code: $LASTEXITCODE"
  }

  .\_build\src\parser\test\run_tests.native .\src\parser\test\flow\
  if ($LASTEXITCODE -gt 0) {
    Throw "flow parser hardcoded ocaml tests exited with error code: $LASTEXITCODE"
  }
} finally {
  popd
}
