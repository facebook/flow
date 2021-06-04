Building Flow from source
Flow is written in OCaml (OCaml 4.09.1 is required).

Install system dependencies:

Mac: brew install opam

Debian: sudo apt-get install opam

Other Linux: see opam docs

Windows: cygwin and a number of dependencies like make, gcc and g++ are required.

One way to install everything is to install Chocolatey and then run .\scripts\windows\install_deps.ps1 and .\scripts\windows\install_opam.ps1. Otherwise, see the "Manual Installation" section of OCaml for Windows docs and install all of the packages listed in our install_deps.ps1.

The remainder of these instructions should be run inside the Cygwin shell: C:\tools\cygwin\Cygwin. Then cd /cygdrive/c/Users/you/path/to/checkout.

