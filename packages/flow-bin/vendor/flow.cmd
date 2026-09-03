::
:: Copyright (c) 2015-present, Facebook, Inc.
:: This source code is licensed under the MIT license found in the
:: LICENSE file in the root directory of this source tree.
::

@IF EXIST "%~dp0\node.exe" (
  "%~dp0\node.exe"  "%~dp0\..\cli.js" %*
) ELSE (
  @SETLOCAL
  @SET PATHEXT=%PATHEXT:;.JS;=;%
  node  "%~dp0\..\cli.js" %*
)
