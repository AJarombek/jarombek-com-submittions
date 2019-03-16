# Learning the basics of Powershell functions
# Author: Andrew Jarombek
# Date: 3/12/2019

# Output of functions is captured and returned.  There is a return keyword, but it just specifies an exit point.
function Mult() {
    param([string]$str, [int]$count)
    $str * $count
}

# Therefore, Mult2 is equivalent to Mult...
function Mult2() {
    param([string]$str, [int]$count)
    return $str * $count
}

# ...and Mult3 is equivalent to Mult2 and Mult
function Mult3() {
    param([string]$str, [int]$count)
    $str * $count
    return
}

# The result is the same when invoking the three functions
$meows = Mult -str "meow" -count 3
Write-Host $meows

$meows2 = Mult2 -str "meow" -count 3
Write-Host $meows2

$meows3 = Mult3 -str "meow" -count 3
Write-Host $meows3