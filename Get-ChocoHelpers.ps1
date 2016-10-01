<# 
.SYNOPSIS
  Get help for chocolatey helper functions.
.PARAMETER 
#>

$parent = (Split-Path -Parent $MyInvocation.MyCommand.Definition)
Write-Host $parent

function Get-ChocoHelpers ($outfile) {
    $parent = (Split-Path -Parent $MyInvocation.MyCommand.Definition)
    
        if ($outfile -eq $null) {
            $outfile = Join-Path 
        }
    
    $path = "$env:chocolateyInstall\helpers\functions"
    Get-ChildItem $path |  %{ . "$path\$_" }
}


