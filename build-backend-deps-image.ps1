$HashCurrent = "$((Get-FileHash server/fitness-tracker.cabal).Hash)$((Get-FileHash ServerDeps.DockerFile).Hash)"

if (Test-Path -Path cache-hash -PathType Leaf) {
  $HashOld = Get-Content -Path cache-hash
} else {
  $HashOld = ""
}

if (
  ($HashCurrent -eq $HashOld) -and
  (((docker images) -join "`n").Contains("fitness-tracker-backend-deps"))
) {
  echo "No Changes in Dependencies"
} else {
  echo "Changes in Dependencies Detected! Rebuilding Dependency Image..."
  docker build --network="host" -t fitness-tracker-backend-deps -f .\ServerDeps.Dockerfile .
  Out-File -FilePath cache-hash -InputObject $HashCurrent
}
