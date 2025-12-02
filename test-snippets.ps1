# Free Pascal Snippet Tester
# Extracts and compiles Pascal snippets from markdown files
# Generates a report of compile successes/failures

param(
    [string]$DocsPath = ".\docs",
    [string]$OutputDir = ".\test-output",
    [string]$FpcBin = "fpc"
)

# Colors for output
$colors = @{
    Complete = "Green"
    Partial = "Yellow"
    Success = "Green"
    Failed = "Red"
    Info = "Cyan"
    Warning = "Yellow"
}

function Write-ColorOutput {
    param([string]$Message, [string]$Color = "White")
    Write-Host $Message -ForegroundColor $Color
}

# Create output directory
if (-not (Test-Path $OutputDir)) {
    New-Item -ItemType Directory -Path $OutputDir | Out-Null
}

Write-ColorOutput "`n========== Free Pascal Snippet Tester ==========" $colors.Info
Write-ColorOutput "Docs Path: $DocsPath`n" $colors.Info

# Find all markdown files
$mdFiles = Get-ChildItem -Path $DocsPath -Filter "*.md" -Recurse

Write-ColorOutput "Found $($mdFiles.Count) markdown files`n" $colors.Info

$snippetCount = 0
$completeCount = 0
$partialCount = 0
$successCount = 0
$failCount = 0
$results = @()

# Process each markdown file
foreach ($file in $mdFiles) {
    $content = Get-Content $file.FullName -Raw
    $relPath = $file.FullName -replace [regex]::Escape($DocsPath), "."

    # Find all pascal code blocks
    $pattern = '```pascal\s*(.*?)```'
    $matches = @([regex]::Matches($content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Singleline))

    if ($matches.Count -eq 0) {
        continue
    }

    $snippetIndex = 0
    foreach ($match in $matches) {
        $snippetIndex++
        if ($null -eq $match) {
            continue
        }
        $snippetCode = $match.Groups[1].Value.Trim()

        # Remove markdown attributes like linenums="1" from the start
        # This regex removes lines that only contain markdown attributes before actual code
        $lines = $snippetCode -split "`n"
        $codeStart = 0
        for ($j = 0; $j -lt $lines.Count; $j++) {
            if ($lines[$j] -match '(program|unit|{|\w)' -and $lines[$j] -notmatch '^[a-zA-Z]+="') {
                $codeStart = $j
                break
            }
        }
        if ($codeStart -gt 0) {
            $snippetCode = ($lines[$codeStart..($lines.Count - 1)] -join "`n").Trim()
        }

        $snippetCount++

        # Determine if snippet is compilable
        # Only PROGRAM declarations can be compiled (not units)
        # Check for: starts with "program" keyword, contains begin/end block
        $hasProgram = $snippetCode -match '^\s*program\s+'
        $hasUnit = $snippetCode -match '^\s*unit\s+(\w+)'
        $hasBegin = $snippetCode -match '\bbegin\b'
        $hasEnd = $snippetCode -match '\bend\s*\.\s*$'
        $isCompilable = $hasProgram -and $hasBegin -and $hasEnd

        $snippetType = "Other"
        $fileName = "{0}_{1:D3}.pas" -f ($file.BaseName), $snippetIndex

        if ($isCompilable) {
            $snippetType = "Program"
            $completeCount++
        } elseif ($hasUnit) {
            $snippetType = "Unit"
            $partialCount++

            # Extract unit name and use it as filename
            if ($snippetCode -match '^\s*unit\s+(\w+)') {
                $unitName = $matches[1]
                $fileName = "$unitName.pas"
            }
        } else {
            $snippetType = "Other"
            $partialCount++
        }

        $testFile = Join-Path $OutputDir $fileName

        # Write the snippet
        $content_to_write = $snippetCode

        # If not compilable and not a unit, wrap in comment to prevent compilation
        if (-not $isCompilable -and $snippetType -ne "Unit") {
            $lines = $snippetCode -split "`n" | ForEach-Object { "  $_" }
            $codeLines = $lines -join "`n"
            $content_to_write = @"
{
  This is a non-compilable snippet from: $relPath (snippet $snippetIndex)

  This snippet cannot be compiled directly.
  It may be: a code fragment, or a teaching example.
  Code:

$codeLines
}
"@
        }

        Set-Content -Path $testFile -Value $content_to_write

        # Compile if this is a compilable program (skip units)
        $compileResult = "SKIPPED"
        $compileError = ""

        if ($isCompilable) {
            $outputExe = Join-Path $OutputDir ([System.IO.Path]::GetFileNameWithoutExtension($fileName))
            $logFile = Join-Path $OutputDir "$fileName.log"

            # Compile
            & $FpcBin -o"$outputExe.exe" -Fu"$OutputDir\libraries" -FuC:\Lazarus\components\lazutils -FuC:\Lazarus\lcl\units\win32 "$testFile" 2>&1 | Tee-Object -FilePath $logFile | Out-Null

            if ($LASTEXITCODE -eq 0) {
                $compileResult = "SUCCESS"
                $successCount++
            } else {
                $compileResult = "FAILED"
                $failCount++
                $compileError = Get-Content $logFile -Raw
            }
        } elseif ($snippetType -eq "Unit") {
            # Units are saved but not compiled - they will be used by other programs
            $compileResult = "SAVED"
        }

        # Store result
        $result = [PSCustomObject]@{
            File = $relPath
            SnippetNum = $i + 1
            Type = $snippetType
            Result = $compileResult
            TestFile = $fileName
            Error = $compileError
        }
        $results += $result
    }
}

# Generate Report
Write-ColorOutput "`n========== SUMMARY ==========" $colors.Info
Write-ColorOutput "Total Snippets: $snippetCount"
Write-ColorOutput "  Programs (compilable): $completeCount" $colors.Complete
Write-ColorOutput "  Other (units/fragments/partial): $partialCount" $colors.Partial

if ($completeCount -gt 0) {
    Write-ColorOutput "`nCompilation Results (Programs):"
    Write-ColorOutput "  ✅ Passed: $successCount"
    Write-ColorOutput "  ❌ Failed: $failCount"

    $passRate = if ($completeCount -gt 0) { [math]::Round(($successCount / $completeCount) * 100, 2) } else { 0 }
    Write-ColorOutput "  Pass Rate: $passRate%`n" $colors.Info
}

$savedUnits = $results | Where-Object { $_.Result -eq "SAVED" } | Measure-Object | Select-Object -ExpandProperty Count
if ($savedUnits -gt 0) {
    Write-ColorOutput "`nUnits Saved:"
    Write-ColorOutput "  💾 Units: $savedUnits`n" $colors.Success
}

# Show snippets by file
Write-ColorOutput "`n========== SNIPPETS BY FILE ==========" $colors.Info

$fileStats = $results | Group-Object -Property File | ForEach-Object {
    $file = $_.Name
    $programs = $_.Group | Where-Object { $_.Type -eq "Program" } | Measure-Object | Select-Object -ExpandProperty Count
    $units = $_.Group | Where-Object { $_.Type -eq "Unit" } | Measure-Object | Select-Object -ExpandProperty Count
    $others = $_.Group | Where-Object { $_.Type -eq "Other" } | Measure-Object | Select-Object -ExpandProperty Count
    $successes = $_.Group | Where-Object { $_.Result -eq "SUCCESS" } | Measure-Object | Select-Object -ExpandProperty Count
    $failures = $_.Group | Where-Object { $_.Result -eq "FAILED" } | Measure-Object | Select-Object -ExpandProperty Count
    $saved = $_.Group | Where-Object { $_.Result -eq "SAVED" } | Measure-Object | Select-Object -ExpandProperty Count

    [PSCustomObject]@{
        File = $file
        Programs = $programs
        Units = $units
        Others = $others
        Success = $successes
        Failed = $failures
        Saved = $saved
    }
}

foreach ($stat in $fileStats) {
    $fileColor = if ($stat.Failed -gt 0) { $colors.Warning } else { $colors.Complete }
    Write-ColorOutput "`n$($stat.File)" $fileColor
    if ($stat.Programs -gt 0) {
        Write-ColorOutput "  Programs: $($stat.Programs)" $colors.Complete
    }
    if ($stat.Units -gt 0) {
        Write-ColorOutput "  Units: $($stat.Units)" $colors.Success
    }
    if ($stat.Others -gt 0) {
        Write-ColorOutput "  Other (fragments): $($stat.Others)" $colors.Partial
    }
    if ($stat.Success -gt 0) {
        Write-ColorOutput "  ✅ Compiled: $($stat.Success)" $colors.Success
    }
    if ($stat.Saved -gt 0) {
        Write-ColorOutput "  💾 Saved: $($stat.Saved)" $colors.Success
    }
    if ($stat.Failed -gt 0) {
        Write-ColorOutput "  ❌ Failed: $($stat.Failed)" $colors.Failed
    }
}

# Show failures
$failures = $results | Where-Object { $_.Result -eq "FAILED" }
if ($failures.Count -gt 0) {
    Write-ColorOutput "`n========== FAILED SNIPPETS ==========" $colors.Warning
    foreach ($failure in $failures) {
        Write-ColorOutput "`n❌ $($failure.File) - Snippet $($failure.SnippetNum)" $colors.Failed
        Write-ColorOutput "   Test File: $($failure.TestFile)"
        if ($failure.Error) {
            $errorLines = $failure.Error -split "`n" | Select-Object -First 5
            foreach ($line in $errorLines) {
                if ($line.Trim()) {
                    Write-ColorOutput "   $line" $colors.Failed
                }
            }
        }
    }
}

# Export detailed results to CSV
$resultsFile = Join-Path $OutputDir "snippet_results.csv"
$results | Export-Csv -Path $resultsFile -NoTypeInformation
Write-ColorOutput "`n✅ Detailed results exported to: $resultsFile" $colors.Success

# Create summary file
$summaryFile = Join-Path $OutputDir "REPORT.txt"

# Build file statistics section
$fileStatsText = ""
foreach ($stat in $fileStats) {
    $fileStatsText += "`n$($stat.File)`n"
    if ($stat.Programs -gt 0) {
        $fileStatsText += "  Programs: $($stat.Programs)`n"
    }
    if ($stat.Units -gt 0) {
        $fileStatsText += "  Units: $($stat.Units)`n"
    }
    if ($stat.Others -gt 0) {
        $fileStatsText += "  Other (fragments): $($stat.Others)`n"
    }
    if ($stat.Success -gt 0) {
        $fileStatsText += "  Compiled: $($stat.Success)`n"
    }
    if ($stat.Saved -gt 0) {
        $fileStatsText += "  Saved: $($stat.Saved)`n"
    }
    if ($stat.Failed -gt 0) {
        $fileStatsText += "  Failed: $($stat.Failed)`n"
    }
}

$savedUnitsText = @()
$results | Where-Object { $_.Type -eq "Unit" } | ForEach-Object {
    $savedUnitsText += "$($_.File) - $($_.TestFile)"
}

$reportContent = @"
FREE PASCAL SNIPPET TEST REPORT
Generated: $(Get-Date)

SUMMARY
-------
Total Snippets: $snippetCount
  Programs (compilable): $completeCount
  Units (for reuse): $($results | Where-Object { $_.Type -eq "Unit" } | Measure-Object | Select-Object -ExpandProperty Count)
  Other (fragments/partial): $partialCount

COMPILATION RESULTS
-------------------
Successful: $successCount
Failed: $failCount
Pass Rate: $(if ($completeCount -gt 0) { "$([math]::Round(($successCount / $completeCount) * 100, 2))%" } else { "N/A" })

UNITS SAVED FOR REUSE
---------------------
$($savedUnitsText | Out-String)

SNIPPETS BY FILE
----------------$fileStatsText

FAILED SNIPPETS
---------------
$($failures | ForEach-Object { "$($_.File) - Snippet $($_.SnippetNum) ($($_.TestFile))" } | Out-String)

TEST FILES LOCATION
-------------------
$OutputDir
"@

Set-Content -Path $summaryFile -Value $reportContent
Write-ColorOutput "`n✅ Summary report saved to: $summaryFile`n" $colors.Success
