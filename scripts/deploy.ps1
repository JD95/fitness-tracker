$ErrorActionPreference = "Stop"

<#
.SYNOPSIS
  This is a helper function that runs a scriptblock and checks the PS variable $lastexitcode
  to see if an error occcured. If an error is detected then an exception is thrown.
  This function allows you to run command-line programs without having to
  explicitly check the $lastexitcode variable.
.EXAMPLE
  exec { svn info $repository_trunk } "Error executing SVN. Please verify SVN command-line client is installed"
#>
function Exec
{
    [CmdletBinding()]
    param(
        [Parameter(Position=0,Mandatory=1)][scriptblock]$cmd,
        [Parameter(Position=1,Mandatory=0)][string]$errorMessage = ("Error executing command {0}" -f $cmd)
    )
    & $cmd
    if ($lastexitcode -ne 0) {
        throw ("Exec: " + $errorMessage)
    }
}

function Generate-Kubernetes-Yaml {
  param (
    [string]$Config
  )
  if (
    (Test-path -Path cache/kubernetes/$Config.hash) -and
    ("$(Exec { dhall hash --file kubernetes/$Config.dhall })" -eq "$(Get-Content -Path cache/kubernetes/$Config.hash)")
    ) {

    Write-Host "- $Config configuration is up to date!" -ForegroundColor Green

  } else {

    Write-Host "- $Config configuration has changed, updating..." -ForegroundColor Yellow
    Exec { dhall resolve --file kubernetes/$Config.dhall | dhall normalize | dhall-to-yaml | Set-Content -Path kubernetes/$Config.yaml }
    Out-File -FilePath ./cache/kubernetes/$Config.hash -InputObject "$(Exec { dhall hash --file kubernetes/$Config.dhall })"
    Write-Host "- $Config configuration is now up to date!" -ForegroundColor Green

  }
}

Try {
  echo "`n[Syncing configuration files]"

  # Setup Caching
  if (!(Test-Path -Path cache/kubernetes)) {
    New-Item -ItemType Directory -Path cache/kubernetes
  }

  Generate-Kubernetes-Yaml -Config "deployment"
  Generate-Kubernetes-Yaml -Config "service"

  Write-Host "`n[Tearing down active deployments]"
  if ("$(kubectl get deployments)".Contains("fitness-tracker-deployment")) {
    Exec { kubectl delete deployment fitness-tracker-deployment }
  }
  if ("$(kubectl get services)".Contains("fitness-tracker-service")) {
    Exec { kubectl delete service fitness-tracker-service }
  }

  Write-Host "`n[Applying deployment configuration]"
  Exec { kubectl apply -f kubernetes/deployment.yaml }

  Write-Host "`n[Applying service configuration]"
  Exec { kubectl apply -f kubernetes/service.yaml }

} Catch {
  throw
}