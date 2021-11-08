$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 3

$dir = Split-Path -Path $(Split-Path -Path $PSCommandPath)

$out = Join-Path $dir out
$compiler = Join-Path $dir 'compiler'
$library = Join-Path $dir 'library'
$test = Join-Path $dir 'test'
$kmkm = Join-Path $out 'kmkm'
$testOut = Join-Path $out 'test'

Push-Location -Path $compiler
try {
  if ($Env:KMKM_TEST_CABAL_PROJECT) {
    cabal install --project $Env:KMKM_TEST_CABAL_PROJECT --installdir $out --overwrite-policy always --enable-executable-dynamic --disable-optimization
  }
  else {
    cabal install --installdir $out --overwrite-policy always --enable-executable-dynamic --disable-optimization
  }
  if ($LASTEXITCODE -ne 0) {
    exit 1
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
    if (Compare-Object (Normalize (Get-Content $filePath)) (Normalize (Get-Content (Join-Path $testOut $case $filePath)))) {
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

$cases = Get-ChildItem -Path (Join-Path $test case) | ForEach-Object { $_.Name }

foreach ($case in $cases) {
  Push-Location -Path (Join-Path $test 'case' $case)
  try {
    & $kmkm -o (Join-Path $testOut $case) -l $library -s compile "$case.s.km"
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
