@echo off
setlocal

set PORT=80

for /f "tokens=2,*" %%A in ('reg query "HKLM\SOFTWARE\R-core\R" /v "InstallPath" 2^>nul') do set "RPATH=%%B"
if not defined RPATH (
    for /f "tokens=2,*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\R-core\R" /v "InstallPath" 2^>nul') do set "RPATH=%%B"
)

if not defined RPATH (
    echo Error: R was not found in the registry.
    pause
    exit /b 1
)

echo R path = %RPATH%

for /f "tokens=2 delims==" %%A in ('wmic cpu get NumberOfLogicalProcessors /value') do (
    set TOTAL_LOGICAL=%%A
)
for /f "tokens=2 delims==" %%A in ('wmic cpu get NumberOfCores /value') do (
    set TOTAL_CORES=%%A
)
set /a PCORES=(TOTAL_LOGICAL-TOTAL_CORES)*2

echo ============================================================
echo  Total Logical cores   : %TOTAL_LOGICAL%
echo  Total Physical cores  : %TOTAL_CORES%
echo  Total P-cores         : %PCORES%
echo ============================================================
echo.

start "" http://127.0.0.1:%PORT%
title RnmrQuant1D
"%RPATH%\bin\Rscript.exe" -e "shiny::runApp(port=%PORT%, launch.browser=FALSE, host='127.0.0.1')"

