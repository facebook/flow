[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$Source,

    [Parameter(Mandatory = $true)]
    [string]$Target)

# This script translates the output from 7zdec into UTF8. Node has limited
# built-in support for encodings.
#
# 7zdec uses the system default code page. The system default code page varies
# depending on the locale configuration. On an en-US box, the system default code
# page is Windows-1252.
#
# Note, on a typical en-US box, testing with the 'ç' character is a good way to
# determine whether data is passed correctly between processes. This is because
# the 'ç' character has a different code point across each of the common encodings
# on a typical en-US box, i.e.
#   1) the default console-output code page (IBM437)
#   2) the system default code page (i.e. CP_ACP) (Windows-1252)
#   3) UTF8

$ErrorActionPreference = 'Stop'

# Redefine the wrapper over STDOUT to use UTF8. Node expects UTF8 by default.
$stdout = [System.Console]::OpenStandardOutput()
$utf8 = New-Object System.Text.UTF8Encoding($false) # do not emit BOM
$writer = New-Object System.IO.StreamWriter($stdout, $utf8)
[System.Console]::SetOut($writer)

# All subsequent output must be written using [System.Console]::WriteLine(). In
# PowerShell 4, Write-Host and Out-Default do not consider the updated stream writer.

Set-Location -LiteralPath $Target

# Print the ##command.
$_7zdec = Join-Path -Path "$PSScriptRoot" -ChildPath "externals/7zdec.exe"
[System.Console]::WriteLine("##[command]$_7zdec x `"$Source`"")

# The $OutputEncoding variable instructs PowerShell how to interpret the output
# from the external command.
$OutputEncoding = [System.Text.Encoding]::Default

# Note, the output from 7zdec.exe needs to be iterated over. Otherwise PowerShell.exe
# will launch the external command in such a way that it inherits the streams.
& $_7zdec x $Source 2>&1 |
    ForEach-Object {
        if ($_ -is [System.Management.Automation.ErrorRecord]) {
            [System.Console]::WriteLine($_.Exception.Message)
        }
        else {
            [System.Console]::WriteLine($_)
        }
    }
[System.Console]::WriteLine("##[debug]7zdec.exe exit code '$LASTEXITCODE'")
[System.Console]::Out.Flush()
if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}