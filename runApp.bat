@echo off
setlocal

set PORT=80

for /f "tokens=2,*" %%A in ('reg query "HKLM\SOFTWARE\R-core\R" /v "InstallPath" 2^>nul') do set "RPATH=%%B"

if not defined RPATH (
    for /f "tokens=2,*" %%A in ('reg query "HKLM\SOFTWARE\WOW6432Node\R-core\R" /v "InstallPath" 2^>nul') do set "RPATH=%%B"
)

if not defined RPATH (
    echo R n'a pas ete trouve dans le registre.
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

for /f %%M in ('powershell -nologo -command "$p=%PCORES%;$t=%TOTAL_LOGICAL%;$pm=([int64]1 -shl $p)-1; $all=([int64]1 -shl $t)-1;$em=$all -bxor $pm;'{0:X}|{1:X}' -f $pm,$em"') do (
    set MASKS=%%M
)
for /f "tokens=1,2 delims=|" %%A in ("%MASKS%") do (
    set AFFINITY=%%A
)

echo ============================================================
echo  Total Logical cores   : %TOTAL_LOGICAL%
echo  Total Physical cores  : %TOTAL_CORES%
echo  Total P-cores         : %PCORES%
echo  Mask                  : 0x%AFFINITY%
echo ============================================================
echo.

start "" http://127.0.0.1:%PORT%
title RnmrQuant1D
"%RPATH%\bin\Rscript.exe" -e "shiny::runApp(port=%PORT%, launch.browser=FALSE, host='127.0.0.1')"
