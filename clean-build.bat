@echo off
REM Clean up build output files
REM This batch file removes compiled artifacts and extracted snippets from the build folder
REM Run this to get a fresh compilation from compile-all-snippets.ps1

setlocal enabledelayedexpansion

if not exist "build" (
    echo build folder does not exist
    exit /b 1
)

echo Cleaning build folder...
echo.

REM Delete .pas files (extracted snippets)
if exist "build\*.pas" (
    del /q "build\*.pas"
    echo Deleted *.pas files
)

REM Delete .o files (object files)
if exist "build\*.o" (
    del /q "build\*.o"
    echo Deleted *.o files
)

REM Delete .ppu files (Free Pascal unit files)
if exist "build\*.ppu" (
    del /q "build\*.ppu"
    echo Deleted *.ppu files
)

REM Delete .exe files (compiled executables)
if exist "build\*.exe" (
    del /q "build\*.exe"
    echo Deleted *.exe files
)

REM Delete .log files (compilation logs)
if exist "build\*.log" (
    del /q "build\*.log"
    echo Deleted *.log files
)

REM Delete CSV results file
if exist "build\snippet_results.csv" (
    del /q "build\snippet_results.csv"
    echo Deleted snippet_results.csv
)

REM Delete text report
if exist "build\REPORT.txt" (
    del /q "build\REPORT.txt"
    echo Deleted REPORT.txt
)

echo.
echo Clean up complete!
endlocal
