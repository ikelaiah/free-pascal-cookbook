@echo off
REM Clean up test output files
REM This batch file removes compiled artifacts and extracted snippets from the test-output folder

setlocal enabledelayedexpansion

if not exist "test-output" (
    echo test-output folder does not exist
    exit /b 1
)

echo Cleaning test-output folder...
echo.

REM Delete .pas files
if exist "test-output\*.pas" (
    del /q "test-output\*.pas"
    echo Deleted *.pas files
)

REM Delete .o files (object files)
if exist "test-output\*.o" (
    del /q "test-output\*.o"
    echo Deleted *.o files
)

REM Delete .ppu files (Free Pascal unit files)
if exist "test-output\*.ppu" (
    del /q "test-output\*.ppu"
    echo Deleted *.ppu files
)

REM Delete .exe files
if exist "test-output\*.exe" (
    del /q "test-output\*.exe"
    echo Deleted *.exe files
)

REM Delete .log files (compilation logs)
if exist "test-output\*.log" (
    del /q "test-output\*.log"
    echo Deleted *.log files
)

echo.
echo Clean up complete!
endlocal
