# Demonstrate equality in PowerShell
# Author: Andrew Jarombek
# Date: 8/17/2019

function Test-Assertion {
<#
.SYNOPSIS
Assertion function for testing statements in PowerShell.

.DESCRIPTION
Assertion function for testing statements in PowerShell.  Returns $true if the 
statement passed as an argument is truthful, throws an error otherwise.

.EXAMPLE
Test-Assertion(2 -eq 2)

.NOTES
Sources: https://gallery.technet.microsoft.com/scriptcenter/A-PowerShell-Assert-d383bf14
#>

    Param(
        [Parameter(Mandatory=$true)]
        [AllowNull()]
        [System.Object]
        $InputStatement
    )

    if ($null -eq $InputStatement) {
        $message = "Assertion Failed - The statement resolves to null"
        Write-Debug -Message $message
        throw $message;
    }
    elseif ($InputStatement -isnot [System.Boolean]) {
        $message = "Assertion Failed - The statement isn't a boolean value"
        Write-Debug -Message $message
        throw $message;
    }
}

[int]$two = 2;
[int]$twoAgain = 2;

Test-Assertion