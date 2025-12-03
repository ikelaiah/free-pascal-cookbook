<#
.SYNOPSIS
    Extracts Free Pascal code snippets from markdown files and tests them.

.DESCRIPTION
    This script:
    1. Scans all markdown files in the docs directory
    2. Extracts Pascal code blocks marked with ```pascal
    3. Automatically generates filenames based on source file and snippet order
    4. Classifies snippets as:
       - Programs: Complete, compilable programs (program keyword + begin/end)
       - Units: Reusable unit declarations (unit keyword)
       - Other: Code fragments, examples, or partial code
    5. Compiles all Programs using the Free Pascal compiler (fpc)
    6. Saves all snippets to the output directory with auto-generated filenames
    7. Generates comprehensive reports and statistics

.PARAMETER DocsPath
    Path to the docs folder containing markdown files (default: .\docs)

.PARAMETER OutputDir
    Output directory for extracted snippets and test results (default: .\test-output)

.PARAMETER FpcBin
    Path to the Free Pascal compiler executable (default: fpc)

.EXAMPLE
    PS> .\test-snippets.ps1
    PS> .\test-snippets.ps1 -DocsPath ".\docs" -OutputDir ".\build" -FpcBin "C:\fpc\bin\fpc.exe"

.NOTES
    Output files:
    - {filename}_{###}.pas - Extracted snippets with auto-generated names
    - snippet_results.csv - Detailed compilation results
    - REPORT.txt - Summary of all test results

    Auto-generated filenames use the pattern: {markdown_base_name}_{snippet_number}.pas
    This allows easy tracking of which snippet came from which documentation file.
#>

param(
    [string]$DocsPath = ".\docs",
    [string]$OutputDir = ".\test-output",
    [string]$FpcBin = "fpc"
)

# Define color scheme for console output
$colors = @{
    Complete = "Green"
    Partial = "Yellow"
    Success = "Green"
    Failed = "Red"
    Info = "Cyan"
    Warning = "Yellow"
}

<#
.SYNOPSIS
    Write colored output to the console

.PARAMETER Message
    The text to display

.PARAMETER Color
    The color to use (default: White)
#>
function Write-ColorOutput {
    param([string]$Message, [string]$Color = "White")
    Write-Host $Message -ForegroundColor $Color
}

# INITIALIZATION
# ==============================================================================

# Create output directory for extracted snippets and reports
if (-not (Test-Path $OutputDir)) {
    New-Item -ItemType Directory -Path $OutputDir | Out-Null
}

Write-ColorOutput "`n========== Free Pascal Snippet Tester ==========" $colors.Info
Write-ColorOutput "Docs Path: $DocsPath`n" $colors.Info

# Find all markdown files that contain Pascal code
$mdFiles = Get-ChildItem -Path $DocsPath -Filter "*.md" -Recurse

Write-ColorOutput "Found $($mdFiles.Count) markdown files`n" $colors.Info

# Initialize counters for summary statistics
$snippetCount = 0        # Total snippets found
$completeCount = 0       # Programs (compilable)
$partialCount = 0        # Units and other snippets
$successCount = 0        # Successfully compiled programs
$failCount = 0           # Failed compilations
$results = @()           # Array to store detailed results

# MARKDOWN PARSING AND SNIPPET EXTRACTION
# ==============================================================================

# Process each markdown file in the docs directory
foreach ($file in $mdFiles) {
    # Read markdown file content
    $content = Get-Content $file.FullName -Raw
    $relPath = $file.FullName -replace [regex]::Escape($DocsPath), "."

    # Extract all Pascal code blocks using regex pattern ```pascal...```
    # Singleline flag allows . to match newlines within code blocks
    # Also capture preceding markdown comments for directives like <!-- SKIP_COMPILE -->
    $pattern = '(<!--\s*SKIP_COMPILE\s*-->\s*)?```pascal\s*(.*?)```'
    $matches = @([regex]::Matches($content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Singleline))

    # Skip files with no Pascal code blocks
    if ($matches.Count -eq 0) {
        continue
    }

    # Process each code block found in this markdown file
    $snippetIndex = 0
    foreach ($match in $matches) {
        $snippetIndex++
        if ($null -eq $match) {
            continue
        }

        # Check for SKIP_COMPILE directive
        # Group 1 captures the optional <!-- SKIP_COMPILE --> marker
        # Group 2 captures the actual Pascal code
        $hasSkipMarker = -not [string]::IsNullOrWhiteSpace($match.Groups[1].Value)

        # Extract code content from regex match and trim whitespace
        $snippetCode = $match.Groups[2].Value.Trim()

        # CLEANUP: Remove markdown attributes (like linenums="1") from code block
        # These are markdown-specific formatting hints that don't belong in Pascal code
        $lines = $snippetCode -split "`n"
        $codeStart = 0
        for ($j = 0; $j -lt $lines.Count; $j++) {
            # Find first line that contains actual Pascal code (not just markdown attributes)
            if ($lines[$j] -match '(program|unit|{|\w)' -and $lines[$j] -notmatch '^[a-zA-Z]+="') {
                $codeStart = $j
                break
            }
        }
        if ($codeStart -gt 0) {
            $snippetCode = ($lines[$codeStart..($lines.Count - 1)] -join "`n").Trim()
        }

        $snippetCount++

        # CLASSIFICATION: Determine snippet type and compilability
        # =========================================================
        # Valid programs must have: "program" keyword + "begin" block + "end."
        # Units have: "unit" keyword (saved but not compiled)
        # Other: fragments, examples, or partial code (cannot be compiled)

        $hasProgram = $snippetCode -match '^\s*program\s+'
        $hasUnit = $snippetCode -match '^\s*unit\s+(\w+)'
        $hasBegin = $snippetCode -match '\bbegin\b'
        $hasEnd = $snippetCode -match '\bend\s*\.\s*$'
        $isCompilable = $hasProgram -and $hasBegin -and $hasEnd

        # If SKIP_COMPILE marker is present, mark as explicitly skipped (not compilable)
        if ($hasSkipMarker) {
            $isCompilable = $false
        }

        # Generate filename: {source_file_base_name}_{snippet_order_number}.pas
        # This creates predictable, traceable filenames for each extracted snippet
        $snippetType = "Other"
        $fileName = "{0}_{1:D3}.pas" -f ($file.BaseName), $snippetIndex

        # Classify snippet and update counters
        if ($isCompilable) {
            $snippetType = "Program"
            $completeCount++
        } elseif ($hasUnit) {
            $snippetType = "Unit"
            $partialCount++

            # For units, use the unit name as filename instead of generic naming
            if ($snippetCode -match '^\s*unit\s+(\w+)') {
                $unitName = $matches[1]
                $fileName = "$unitName.pas"
            }
        } else {
            $snippetType = "Other"
            $partialCount++
        }

        # FILE WRITING
        # ==============================================================================

        $testFile = Join-Path $OutputDir $fileName
        $content_to_write = $snippetCode

        # For non-compilable snippets (fragments, examples), wrap in comment block
        # This preserves them for reference but prevents accidental compilation attempts
        if (-not $isCompilable -and $snippetType -ne "Unit") {
            $lines = $snippetCode -split "`n" | ForEach-Object { "  $_" }
            $codeLines = $lines -join "`n"

            $skipReason = if ($hasSkipMarker) {
                "This snippet is explicitly marked with <!-- SKIP_COMPILE -->"
            } else {
                "This snippet cannot be compiled directly.`n  It may be: a code fragment, or a teaching example."
            }

            $content_to_write = @"
{
  This is a non-compilable snippet from: $relPath (snippet $snippetIndex)

  $skipReason
  Code:

$codeLines
}
"@
        }

        # Save the snippet to file
        Set-Content -Path $testFile -Value $content_to_write

        # COMPILATION
        # ==============================================================================

        # Initialize compilation result tracking
        $compileResult = "SKIPPED"
        $compileError = ""

        # Compile only Programs (not Units or Other fragments)
        if ($isCompilable) {
            $outputExe = Join-Path $OutputDir ([System.IO.Path]::GetFileNameWithoutExtension($fileName))
            $logFile = Join-Path $OutputDir "$fileName.log"

            # Run Free Pascal compiler with Lazarus library paths for dependencies
            # Output redirected to log file for later inspection if compilation fails
            & $FpcBin -o"$outputExe.exe" -Fu"$OutputDir\libraries" -FuC:\Lazarus\components\lazutils -FuC:\Lazarus\lcl\units\win32 "$testFile" 2>&1 | Tee-Object -FilePath $logFile | Out-Null

            # Check compilation result and update counters
            if ($LASTEXITCODE -eq 0) {
                $compileResult = "SUCCESS"
                $successCount++
            } else {
                $compileResult = "FAILED"
                $failCount++
                $compileError = Get-Content $logFile -Raw
            }
        } elseif ($snippetType -eq "Unit") {
            # Units are extracted and saved but not compiled
            # They can be referenced and included in other programs
            $compileResult = "SAVED"
        }

        # RESULT TRACKING
        # ==============================================================================

        # Record all snippet metadata for final reports
        $result = [PSCustomObject]@{
            File = $relPath
            SnippetNum = $snippetIndex
            Type = $snippetType
            Result = $compileResult
            TestFile = $fileName
            Error = $compileError
        }
        $results += $result
    }
}

# REPORT GENERATION
# ==============================================================================

# CONSOLE SUMMARY: Display compilation statistics to the user
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

# DETAILED STATISTICS: Show breakdowns by source file
Write-ColorOutput "`n========== SNIPPETS BY FILE ==========" $colors.Info

# Calculate statistics grouped by source markdown file
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

# FAILURE DETAILS: Show any compilation errors for quick debugging
$failures = $results | Where-Object { $_.Result -eq "FAILED" }
if ($failures.Count -gt 0) {
    Write-ColorOutput "`n========== FAILED SNIPPETS ==========" $colors.Warning
    foreach ($failure in $failures) {
        Write-ColorOutput "`n❌ $($failure.File) - Snippet $($failure.SnippetNum)" $colors.Failed
        Write-ColorOutput "   Test File: $($failure.TestFile)"
        if ($failure.Error) {
            # Show first 5 lines of error output for brevity (full logs in files)
            $errorLines = $failure.Error -split "`n" | Select-Object -First 5
            foreach ($line in $errorLines) {
                if ($line.Trim()) {
                    Write-ColorOutput "   $line" $colors.Failed
                }
            }
        }
    }
}

# FILE EXPORTS: Create permanent records of all test results for CI/CD integration
Write-ColorOutput "`n========== GENERATING REPORTS ==========" $colors.Info
$resultsFile = Join-Path $OutputDir "snippet_results.csv"
$results | Export-Csv -Path $resultsFile -NoTypeInformation
Write-ColorOutput "`n✅ Detailed results exported to: $resultsFile" $colors.Success

# Generate comprehensive text report
$summaryFile = Join-Path $OutputDir "REPORT.txt"

# Build detailed file statistics section for text report
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
