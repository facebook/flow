
REM No echo arguments will be printed in the current state
@echo off

REM Set env variables
SET TERM=dumb
SET EXITCODE=0

REM Run ocaml.exe
ocaml.exe -I scripts -w -3 str.cma unix.cma .\scripts\ocp_build_glob.ml ocp_build_hack.ocp.fb ocp_build_hack.ocp
ocaml.exe -I scripts -w -3 str.cma unix.cma .\scripts\ocp_build_glob.ml ocp_build_flow.ocp.fb ocp_build_flow.ocp

if "%1" == "" goto build
if "%1" == "build" goto build
if "%1" == "all" goto build
if "%1" == "init" goto init
if "%1" == "clean" goto clean
if "%1" == "test" goto test 
REM Invalid argument
echo Invalid argument, please check README.win32
goto end

REM initialize ocp-build and create _obuild directory
:init
if not exist "_obuild/" ocp-build init
goto end

REM 1/ check if ocp-build init has already be done
REM 2/ generate get_build_id.gen.c
REM 3/ start build hack with ocp-build
:build
if not exist "_obuild/" ocp-build init
ocaml.exe -I scripts -w -3 unix.cma .\scripts\gen_build_id.ml .\hack\utils\get_build_id.gen.c 
ocaml.exe -I scripts -w -3 unix.cma .\scripts\gen_index.ml flowlib.rc lib
ocp-build
REM If the build failed then give up
if %ERRORLEVEL% neq 0 (
  SET EXITCODE=%ERRORLEVEL%
  goto build_end
)
md bin 2>NUL
copy _obuild\flow\flow.asm.exe bin\flow.exe

:build_end
del lib\INDEX

goto end

REM clean _obuild directory and executables in bin/
:clean
ocp-build clean
del bin\flow.exe 2>NUL
goto end

REM execute the Flow testsuite
:test
ocp-build tests

:end
del ocp_build_flow.ocp
del ocp_build_hack.ocp
exit /B %EXITCODE%
