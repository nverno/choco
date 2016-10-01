<# 
.SYNOPSIS
  Get help for chocolatey helper functions.
#>

$parent = (Split-Path -Parent $MyInvocation.MyCommand.Definition)
Write-Host $parent

function Get-ChocoHelpers ($outfile=$null) {
    $parent = (Split-Path -Parent $MyInvocation.MyCommand.Definition)
    
        if ($outfile -eq $null) {
            $outfile = Join-Path 
        }
    
    $path = "$env:chocolateyInstall\helpers\functions"
    Get-ChildItem $path |  %{ . "$path\$_" }
}


