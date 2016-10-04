<# 
.SYNOPSIS
Get help for chocolatey helper functions.
.PARAMETER helpers
Powershell helper script.
#>

# $parent = (Split-Path $SCRIPT:MyInvocation.MyCommand.Path -parent)
$helpers = "..\powershell\build\helpers.psm1"

function Get-ChocoHelpers ($helpers, $outfile) {
  if ($helpers -eq $null -or !(Test-Path $helpers)) {
    Write-Error "Unable to load $helpers."
  }
  if ($outfile -eq $null) {
    $parent = (Split-Path -Parent $MyInvocation.MyCommand.Definition)
    $outfile = Join-Path $parent "choco-data.el"
  }
  Import-Module "$helpers" -Force
  $scriptdir = "$env:ChocolateyInstall\helpers\functions"

  foreach ($script in $(Get-ChildItem $scriptdir)) {
    . "$scriptdir\$script"
    $spath = [System.IO.Path]::GetFullPath()
    Get-Command .($script.Basename) | Write-EmacsFunction "choco-hash"
  }
  # Get-ChildItem $path |  %{ . "$path\$_" }
}
