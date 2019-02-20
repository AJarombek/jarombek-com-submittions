@echo off

:: Author: Andrew Jarombek
:: Date: 7/27/2018
:: Exploring Batch Scripting Function Basics.

setlocal enableDelayedExpansion

call :stringTimes cat 3
echo hey
echo %result%
goto :eof

:stringTimes - A function to multiply a string a certain number of times and return the result
    setlocal

    set string=%1
    set /a count=%2
    set finalString=%string%

    :stringTimesLoop
    if %count% gtr 1 (
        set finalString=%finalString%%string%
        set /a count=%count% - 1
        goto :stringTimesLoop
    )

    endlocal & set result=%finalString%
    goto :eof