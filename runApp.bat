@echo off
setlocal

@echo off
if "%~1"=="MINIMIZED" goto :RUN
start "" /min cmd /c "%~f0" MINIMIZED
exit /b

:RUN
set PORT=8080

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

start ""  http://127.0.0.1:%PORT%
REM start "" "C:\Program Files\BraveSoftware\Brave-Browser\Application\brave.exe" http://127.0.0.1:%PORT%
REM start "" "C:\Program Files\Mozilla Firefox\firefox.exe" http://127.0.0.1:%PORT%
REM start "" "C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe" http://127.0.0.1:%PORT%
title RnmrQuant1D
"%RPATH%\bin\Rscript.exe" -e "shiny::runApp(appDir=file.path(getwd(),'app'), port=%PORT%, launch.browser=FALSE, host='127.0.0.1')"
