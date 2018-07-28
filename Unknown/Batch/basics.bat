@echo off
rem Turning Echo off ensures that commands wont display when the batch script executes
rem Note: The REM command is used for documentation and lines starting with REM will be
rem     ignored when the command executes.  You can also use the '::' syntax for comments

:: Author: Andrew Jarombek
:: Date: 7/26/2018
:: Exploring Batch Scripting Basics.

:: For arguments passed in to a batch script, the '%#' notation is used.
:: This command prints out the first argument passed to the batch script.
echo The first command passed: %1

:: Define and Initialize a new variable.  Referencing the variable with the %variable-name% syntax
set name=Andrew Jarombek
echo %name%

:: Extract a substring starting at character 0 and ending at character 6
set firstName=%name:~0,6%
echo "%firstName%"

:: Alternative way to get the first name with negative character indexing
set firstName=%name:~0,-9%
echo "%firstName%"

:: Extract a substring of all characters beyond position 7
set lastName=%name:~7%
echo "%lastName%"

:: String replacement with ':substringToRemove=replacementString' notation
set fakeName=%name:Jarombek=Jarbek%
echo %fakeName%

:: Use '/A' for numeric values
set /a age=23
set /a ten=10

set /a ageInTenYears=%age% + %ten%
echo Age in 10 Years: %ageInTenYears%

:: Define a local scope.  Any variables defined in the local scope will no
::     longer be visible once returning to the global scope.
setlocal
set age=24
echo Age within Local Scope: %age%
endlocal

echo Age outside of Local Scope: %age%

:: Global Variable
echo %JAVA_HOME%

:: Create an array by assigning a value to an index
set towns[0]=Greenwich
set towns[1]=New Canaan
set towns[2]=Darien
set towns[3]=Wilton
set towns[4]=Ridgefield

:: Access the value at the first array index
echo %towns[0]%

set numberList=0 1 2 3 4

:: Allow for variables within a batch file to be expanded at execution time instead of parse time
:: The syntax !variable! will expand the variable when the line is executed -
:: for example during each iteration of the for loop.  With the %variable% syntax the value is expanded
:: at parse time instead.
setlocal enableDelayedExpansion

for %%i in (%numberList%) do (
    set allTowns=!allTowns! !towns[%%i]!
)

echo %allTowns%

:: Alternative loop syntax with the array defined within the loop
for %%i in (0 2 4) do (
    set someTowns=!someTowns! !towns[%%i]!
)

echo %someTowns%

:: Array elements can actually be complex structures with different named properties
set restaurant[0].name=Little Pub
set restaurant[0].location=Cos Cob, CT

set restaurant[1].name=Bobby Vs
set restaurant[1].location=Stamford, CT

echo The restaurant %restaurant[0].name% is located in %restaurant[0].location%
echo The restaurant %restaurant[1].name% is located in %restaurant[1].location%

:: Basic if..else logic
if %towns[0]% == Greenwich (
    echo The best town is Greenwich^^!
) else (
    echo Something must have gone wrong^^!
)

:: Batch has DATE and TIME variables
echo %DATE%

:: LSS is the less than operator - Others include:
:: EQU (equal to), NEQ (not equal to), LEQ (less than or equal to), GTR (greater than), GEQ (greater than or equal to)
if "%DATE%" lss "Fri 07/28/2018" (
    echo %DATE% is before Saturday July 28th, 2018
) else (
    echo %DATE% is equal to or later than Saturday July 28th, 2018
)

:: Create a function to print out the current time.  The exit commands last item (0) is the error code
:: An error code of 0 means success
:displayTime
    echo The current time is %TIME%
    exit /b 0

:: Invoke the created function
call :displayTime