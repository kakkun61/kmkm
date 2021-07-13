$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 3

$dir = Split-Path -Path $(Split-Path -Path $PSCommandPath)
$out = "$dir\out"
$compiler = "$dir\compiler"
$library = "$dir\library"
$test = "$dir\test"
$kmkm = "$out\kmkm"
$testOut = "$out\test"

Push-Location -Path $compiler
try {
  cabal install --installdir $out --overwrite-policy always
  if ($LASTEXITCODE -ne 0) {
    exit
  }
}
finally {
  Pop-Location
}

$fail = $false

function Compare-Result {
  param (
    [string] $case,
    [string] $filePath
  )

  Write-Host "`t$filePath " -NoNewline
  try {
    if (Compare-Object (Normalize (Get-Content $filePath)) (Normalize (Get-Content "$testOut\$case\$filePath"))) {
      Write-Host "FAIL" -ForegroundColor Red
      $script:fail = $true
    }
    else {
      Write-Host "OK" -ForegroundColor Green
    }
  }
  catch {
    Write-Host "FAIL" -ForegroundColor Red
    Write-Host $_
  }
}

function Normalize {
  param (
    [string[]] $lines
  )
  $lines | ForEach-Object { if ($_ -ne $null) { $_.Trim() } else { $_ } } | Where-Object { $_ -ne "" }
}

$cases = Get-ChildItem -Path "$test\case" | ForEach-Object { $_.Name }

foreach ($case in $cases) {
  Push-Location -Path "$test\case\$case"
  try {
    & $kmkm -o "$testOut\$case" -l $library "$case.s.km"
    Write-Host "$case " -NoNewline
    if ($LASTEXITCODE -ne 0) {
      Write-Host "ERROR" -ForegroundColor Red
      $fail = $true
      continue
    }
    Write-Host "COMPILED" -ForegroundColor Green
    Compare-Result $case "$case.c"
    Compare-Result $case "$case.h"
  }
  finally {
    Pop-Location
  }
}

if ($fail) {
  exit 1
}
