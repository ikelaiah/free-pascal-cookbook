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

        # Determine if snippet is complete (has boilerplate)
        # Check for: starts with program/unit, contains begin, ends with end.
        $hasProgram = $snippetCode -match '^\s*(program|unit)\s+'
        $hasBegin = $snippetCode -match '\bbegin\b'
        $hasEnd = $snippetCode -match '\bend\s*\.\s*$'
        $isComplete = $hasProgram -and $hasBegin -and $hasEnd

        if ($isComplete) {
            $snippetType = "Complete"
            $completeCount++
        } else {
            $snippetType = "Partial"
            $partialCount++
        }

        # Create test file
        $fileName = "{0}_{1:D3}.pas" -f ($file.BaseName), $snippetIndex
        $testFile = Join-Path $OutputDir $fileName

        # Write the snippet (complete snippets are compiled as-is, partial ones are not tested for compilation)
        $content_to_write = $snippetCode

        # If partial, wrap in comment to prevent compilation
        if (-not $isComplete) {
            $lines = $snippetCode -split "`n" | ForEach-Object { "  $_" }
            $codeLines = $lines -join "`n"
            $content_to_write = @"
{
  This is a partial snippet from: $relPath (snippet $snippetIndex)

  Partial snippets are not compiled by default.
  Code:

$codeLines
}
"@
        }

        Set-Content -Path $testFile -Value $content_to_write

        # Compile if complete
        $compileResult = "SKIPPED"
        $compileError = ""

        if ($isComplete) {
            $outputExe = Join-Path $OutputDir ([System.IO.Path]::GetFileNameWithoutExtension($fileName))
            $logFile = Join-Path $OutputDir "$fileName.log"

            # Compile
            & $FpcBin -o"$outputExe.exe" -FuC:\Lazarus\components\lazutils -FuC:\Lazarus\lcl\units\win32 "$testFile" 2>&1 | Tee-Object -FilePath $logFile | Out-Null

            if ($LASTEXITCODE -eq 0) {
                $compileResult = "SUCCESS"
                $successCount++
            } else {
                $compileResult = "FAILED"
                $failCount++
                $compileError = Get-Content $logFile -Raw
            }
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
Write-ColorOutput "  Complete (should compile): $completeCount" $colors.Complete
Write-ColorOutput "  Partial (incomplete): $partialCount" $colors.Partial

if ($completeCount -gt 0) {
    Write-ColorOutput "`nCompilation Results (Complete Snippets):"
    Write-ColorOutput "  ✅ Passed: $successCount"
    Write-ColorOutput "  ❌ Failed: $failCount"

    $passRate = if ($completeCount -gt 0) { [math]::Round(($successCount / $completeCount) * 100, 2) } else { 0 }
    Write-ColorOutput "  Pass Rate: $passRate%`n" $colors.Info
}

# Show snippets by file
Write-ColorOutput "`n========== SNIPPETS BY FILE ==========" $colors.Info

$fileStats = $results | Group-Object -Property File | ForEach-Object {
    $file = $_.Name
    $completes = $_.Group | Where-Object { $_.Type -eq "Complete" } | Measure-Object | Select-Object -ExpandProperty Count
    $partials = $_.Group | Where-Object { $_.Type -eq "Partial" } | Measure-Object | Select-Object -ExpandProperty Count
    $successes = $_.Group | Where-Object { $_.Result -eq "SUCCESS" } | Measure-Object | Select-Object -ExpandProperty Count
    $failures = $_.Group | Where-Object { $_.Result -eq "FAILED" } | Measure-Object | Select-Object -ExpandProperty Count

    [PSCustomObject]@{
        File = $file
        Complete = $completes
        Partial = $partials
        Success = $successes
        Failed = $failures
    }
}

foreach ($stat in $fileStats) {
    $fileColor = if ($stat.Failed -gt 0) { $colors.Warning } else { $colors.Complete }
    Write-ColorOutput "`n$($stat.File)" $fileColor
    Write-ColorOutput "  Complete: $($stat.Complete)" $colors.Complete
    if ($stat.Partial -gt 0) {
        Write-ColorOutput "  Partial: $($stat.Partial)" $colors.Partial
    }
    if ($stat.Success -gt 0) {
        Write-ColorOutput "  ✅ Compiled: $($stat.Success)" $colors.Success
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
    $fileStatsText += "  Complete: $($stat.Complete)`n"
    if ($stat.Partial -gt 0) {
        $fileStatsText += "  Partial: $($stat.Partial)`n"
    }
    if ($stat.Success -gt 0) {
        $fileStatsText += "  Compiled: $($stat.Success)`n"
    }
    if ($stat.Failed -gt 0) {
        $fileStatsText += "  Failed: $($stat.Failed)`n"
    }
}

$reportContent = @"
FREE PASCAL SNIPPET TEST REPORT
Generated: $(Get-Date)

SUMMARY
-------
Total Snippets: $snippetCount
  Complete (with boilerplate): $completeCount
  Partial (incomplete): $partialCount

COMPILATION RESULTS
-------------------
Successful: $successCount
Failed: $failCount
Pass Rate: $(if ($completeCount -gt 0) { "$([math]::Round(($successCount / $completeCount) * 100, 2))%" } else { "N/A" })

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
