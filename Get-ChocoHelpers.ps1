<# 
.SYNOPSIS
Get help for chocolatey helper functions.
.PARAMETER outfile
Name of output file.
#>

# $parent = (Split-Path $SCRIPT:MyInvocation.MyCommand.Path -parent)

function Get-ChocoHelpers ($outfile) {
    if ($outfile -eq $null) {
        $outfile = Join-Path $parent helpers.el
    }

    $path = "$env:chocolateyInstall\helpers\functions"
    foreach ($s in $(Get-ChildItem $path)) {
        . "$path\$s"
        Get-Help $s.BaseName
        # .$path\$s
        # Get-Help $path\$s
        # Get-Help "$path\$s"
    }
    # Get-ChildItem $path |  %{ . "$path\$_" }
}
