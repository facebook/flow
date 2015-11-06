
@echo off

if "%1" == "" goto build
if "%1" == "build" goto build
if "%1" == "all" goto build
if "%1" == "init" goto init
if "%1" == "clean" goto clean
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
ocaml.exe unix.cma .\scripts\gen_build_id.ml .\hack\utils\get_build_id.gen.c
ocaml.exe unix.cma .\scripts\gen_index.ml flowlib.rc lib
ocp-build
md bin 2>NUL
copy _obuild\flow\flow.asm.exe bin\flow.exe

goto end

REM clean _obuild directory and executables in bin/
:clean
ocp-build clean
del bin\flow.exe 2>NUL
goto end

REM execute the Flow testsuite

:end
