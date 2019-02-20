# Learning the basics of Powershell
# Author: Andrew Jarombek
# Date: 2/7/2019

# If this returns 'Restricted', the execution policy must be changed to run Powershell scripts
Get-ExecutionPolicy

Set-ExecutionPolicy RemoteSigned

$log = "Hello Log"
$log | Out-File -FilePath C:\output.txt
