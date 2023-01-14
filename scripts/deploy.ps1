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
        [Parameter(Position=0,Mandatory=1)][scriptblock]$Cmd,
        [Parameter(Position=1,Mandatory=0)][string]$ErrorMessage = ("Error executing command {0}" -f $Cmd)
    )
    & $Cmd
    if ($lastexitcode -ne 0) {
        throw ("Exec: " + $ErrorMessage)
    }
}

function Generate-Kubernetes-Yaml {
  param (
    [string]$Config
  )
  if (
    (Test-path -Path "cache/kubernetes/$Config.hash") -and
    ("$(Exec { dhall hash --file kubernetes/$Config.dhall })" -eq "$(Get-Content -Path cache/kubernetes/$Config.hash)")
    ) {

    Write-Host "- $Config configuration is up to date!" -ForegroundColor Green
    return $false

  } else {

    Write-Host "- $Config configuration has changed, updating..." -ForegroundColor Yellow
    Exec { dhall resolve --file "kubernetes/$Config.dhall" | dhall normalize | dhall-to-yaml | Set-Content -Path "kubernetes/$Config.yaml" }
    Out-File -FilePath "./cache/kubernetes/$Config.hash" -InputObject "$(Exec { dhall hash --file kubernetes/$Config.dhall })"
    return $true

  }
}

function Sync-Pod {
  param (
    [string]$PodName
  )

  if (!(Test-Path -Path "cache/kubernetes/$PodName")) {
    Write-Host "- Creating cache folder for $Podname"
    New-Item -ItemType Directory -Path "cache/kubernetes/$Podname"
  }

  echo "`n[Syncing configuration for $PodName]"
  if (Generate-Kubernetes-Yaml -Config "$PodName/deployment") {
    Write-Host "- Deleting existing deployment for $PodName"
    if ("$(kubectl get deployments)".Contains("$PodName")) {
      Exec { kubectl delete deployment "$PodName" }
    }
  }
  Exec { kubectl apply -f "kubernetes/$PodName/deployment.yaml" }

  if (Generate-Kubernetes-Yaml -Config "$PodName/service") {
    Write-Host "- Deleting existing service for $PodName"
    if ("$(kubectl get services)".Contains("$PodName")) {
      Exec { kubectl delete service "$PodName" }
    }
  }
  Exec { kubectl apply -f "kubernetes/$PodName/service.yaml" }
}

Try {

  # Setup Caching
  if (!(Test-Path -Path cache/kubernetes)) {
    Write-Host "Creating cache for kubernetes"
    New-Item -ItemType Directory -Path cache/kubernetes
  }

  Sync-Pod "fitness-tracker"
  Sync-Pod "authentication"

} Catch {
  throw
}