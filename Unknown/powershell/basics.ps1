# Learning the basics of Powershell
# Author: Andrew Jarombek
# Date: 2/7/2019

# Handle script arguments
param (
    [string]$author = "Andrew Jarombek"
)

Write-Host $author

# If this returns 'Restricted', the execution policy must be changed to run Powershell scripts
Get-ExecutionPolicy

# Set-ExecutionPolicy RemoteSigned

# Create a variable named log and output its value to a file named output.txt
$log = "Hello Log"
$log | Out-File -FilePath C:\output.txt

# Write-Host is equivalent to 'echo'
Write-Host $env:Path

# Select just the lastname portion of the author string
$lastname = $author.Substring(7)
Write-Host $lastname

# ...this method checks if author contains "Jarombek"
$lastname2 = $author -match "Jarombek"
Write-Host $lastname2

# Replace Jarombek with Jarbek
$altauthor = $author.Replace("Jarombek", "Jarbek")
Write-Host $altauthor

# Perform addition.  Powershell is simplest amongst Bash & Batch
$age = 23
$ageInTenYears = $age + 10
Write-Host $ageInTenYears

function Test-Scoping() {
    # Variables are scoped locally to the function
    $age = 24
    Write-Host $age
}

Test-Scoping
Write-Host $age

function Test-Scoping-Variables() {
    # By default, variables created in functions are local
    $countLocal = 0

    # By using the $script: prefix, variables are persisted across function calls
    $script:countScript = 0

    # By using the $global: prefix, variables become global
    $global:countGlobal = 0

    Write-Host "Local count: $countLocal, Script count: $script:countScript, Global count: $global:countGlobal"
}

Test-Scoping-Variables
Write-Host "Local count: $countLocal, Script count: $countScript, Global count: $countGlobal"

# Powershell array creation is also simple
$towns = @("Greenwich","Darien","New Canaan","Wilton","Ridgefield")
Write-Host $towns[0]

# Shortened syntax to create an array of a number range
$numberArray = @(0..10)
$subArray = $numberArray[5..9]

# Collect all the towns north of I-95
$northernTownsArray = $towns[2..4]
$northernTownsString = ""

foreach ($element in $northernTownsArray) {
    $northernTownsString += $element + " "
}

Write-Host $northernTownsString

# An alternative way to iterate over an array is to use a pipe
$coastalTownsArray = $towns[0..1]
$coastalTownsString = ""

$coastalTownsArray | foreach {
    $coastalTownsString += $_ + " "
}

Write-Host $coastalTownsString

# PowerShell supports mult-dimensional arrays
$multiDimensional = @(
    @("Andrew", "Jarombek"),
    @("Tom", "Caul"),
    @("Joseph", "Smoth")
)

$tom = $multiDimensional[1][0]
Write-Host $tom

# Variables in PowerShell can have explicit types
[string[]]$names = @("Andy", "Tom", "Joe")

[DateTime]$CurrentDate = Get-Date -DisplayHint Date

# Demonstrate that there is a lot of functionality availiable on a DateTime object
Write-Host "It is currently Daylight Savings Time: $($CurrentDate.IsDaylightSavingTime())"
Write-Host "Current Month: $($CurrentDate.Month)"
Write-Host "Next Month: $($CurrentDate.AddMonths(1).Month)"

Write-Host $CurrentDate

[DateTime]$EndOfMonth = (Get-Date -Year 2019 -Month 03 -Day 31)

# Its exremely simple to compare dates
if ($CurrentDate -GT $EndOfMonth) {
    Write-Host "It isn't March Anymore: $CurrentDate"
} else {
    Write-Host "It's still March: $CurrentDate"
}