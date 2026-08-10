param(
  [Parameter(Mandatory = $true)]
  [ValidateSet('main', 'bias')]
  [string]$Experiment
)

$ErrorActionPreference = 'Stop'
$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$driveRoot = [System.IO.Path]::GetPathRoot($repoRoot)
$runtimeRoot = Join-Path $driveRoot 'Codex work\R-runtime'
$tempRoot = Join-Path $runtimeRoot 'temp'
$renvRoot = Join-Path $runtimeRoot 'renv'
$renvCache = Join-Path $renvRoot 'cache'

New-Item -ItemType Directory -Force -Path $tempRoot, $renvRoot, $renvCache | Out-Null

$env:TEMP = $tempRoot
$env:TMP = $tempRoot
$env:TMPDIR = $tempRoot
$env:RENV_PATHS_ROOT = $renvRoot
$env:RENV_PATHS_CACHE = $renvCache

$projects = @{
  main = Join-Path $repoRoot 'experiments\main_three_learners\ThreeLearnersIsoCrossCalibration.Rproj'
  bias = Join-Path $repoRoot 'experiments\bias_decomposition\BiasDecompositionExperiment.Rproj'
}
$projectFile = $projects[$Experiment]
$projectDir = Split-Path -Parent $projectFile

$rstudioCommand = Get-Command rstudio.exe -ErrorAction SilentlyContinue
$candidates = @()
if ($null -ne $rstudioCommand) {
  $candidates += $rstudioCommand.Source
}
$candidates += @(
  'C:\Program Files\RStudio\rstudio.exe',
  'C:\Program Files\RStudio\bin\rstudio.exe',
  'C:\Program Files\Posit\RStudio\rstudio.exe',
  (Join-Path $env:LOCALAPPDATA 'Programs\RStudio\rstudio.exe')
)
$rstudio = $candidates | Where-Object { $_ -and (Test-Path -LiteralPath $_ -PathType Leaf) } | Select-Object -First 1

if (-not $rstudio) {
  throw 'RStudio executable was not found. Add rstudio.exe to PATH or edit the candidate list in this script.'
}

Write-Host "R temporary directory: $tempRoot"
Write-Host "renv root/cache: $renvRoot"
Write-Host "Opening project: $projectFile"
Start-Process -FilePath $rstudio -ArgumentList ('"' + $projectFile + '"') -WorkingDirectory $projectDir
