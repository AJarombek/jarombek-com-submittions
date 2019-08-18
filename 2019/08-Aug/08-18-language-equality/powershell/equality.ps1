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
    elseif (-not $InputStatement) {
        $message = "Assertion Failed - The statement resolves to `$false"
        Write-Debug -Message $message
        throw $message;
    }
    Write-Verbose -Message "Assertion Passed!"
}

[int] $two = 2;
[int] $twoAgain = 2;

Test-Assertion ($two -eq $twoAgain)

# PowerShell does have type coercion when testing for equality.
Test-Assertion ($two -eq "2")

Test-Assertion ($two -ne 3)
Test-Assertion ($two -ne "3")

# If the first argument to -eq is a list of values, the result is all the 
# values that match the second argument. 
[Object[]] $resultsInList = 2,3,4 -eq 2;
Test-Assertion ($resultsInList.Length -eq 1)

[int] $resultsInTwo = $resultsInList[0];
Test-Assertion ($resultsInTwo -eq 2)

# Equality works with strings as expected.
[string] $name = "Andy";
[string] $nameAgain = "Andy";
[string] $lastName = "Jarombek";

Test-Assertion ($name -eq $nameAgain)
Test-Assertion ($name -ne $lastName)

# PowerShell uses the .NET framework just like C#.  Therefore, it has 
# access to Object.Equals() and Object.ReferenceEquals().
# While $name and $nameAgain pass the value equality test, they fail the 
# reference equality test.
Test-Assertion([System.Object]::Equals($name, $nameAgain))
Test-Assertion(-not [System.Object]::ReferenceEquals($name, $nameAgain))

Test-Assertion([System.Object]::Equals($two, $twoAgain))
Test-Assertion(-not [System.Object]::ReferenceEquals($two, $twoAgain))

class Yarn {
    [string] $fiber;
    [string] $color;
    [int] $yards;

    Yarn ([string] $fiber, [string] $color, [int] $yards) {
        $this.fiber = $fiber;
        $this.color = $color;
        $this.yards = $yards;
    }

    [bool] Equals([System.Object] $other) {
        [bool] $fibersEqual = $this.fiber -eq $other.fiber
        [bool] $colorsEqual = $this.color -eq $other.color
        [bool] $yardsEqual = $this.yards -eq $other.yards
        return $fibersEqual -and $colorsEqual -and $yardsEqual;
    }
}

$yarn1 = [Yarn]::new("Polyester", "Pitter Patter", 210);
$yarn2 = $yarn1;