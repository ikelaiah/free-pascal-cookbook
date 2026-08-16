# Roadmap

This document outlines the planned direction for the Free Pascal Cookbook. It
complements [`CHANGELOG.md`](CHANGELOG.md): the changelog records completed work,
while this file describes the next outcomes and the criteria for reaching them.

The roadmap is intentionally short near `1.0.0`. Later ideas are grouped by
theme rather than assigned speculative version numbers. Priorities may change as
reader feedback exposes more important gaps.

## Current Status

- **Content:** Basics, Core Tasks, Advanced Topics, External Systems, Resources,
  and Community sections are published. The documented and CI-tested baseline is
  FPC 3.2.2 with Lazarus 4.0 where Lazarus is needed. A full page-by-page
  technical review remains planned.
- **Tooling:** `compile-all-snippets.ps1` extracts every `pascal` code block. It
  compiles complete programs, saves units for reuse, and reports fragments and
  explicitly skipped examples without claiming that they were compiled.
- **CI:** MkDocs strict builds run on Linux, and complete Pascal programs are
  compiled on Windows and Linux. Snippet reports are uploaded as workflow
  artifacts.
- **Quality:** Complete programs currently compile successfully in the tested
  environments. Compilation alone does not prove that a program runs correctly,
  that every fragment is valid in context, or that every explanation is current.

## Vision

The cookbook should be a trusted, approachable place to learn Free Pascal for
two audiences:

1. **New FPC developers** who need a clear path from "Hello, World!" to useful
   programs.
2. **Developers coming from other languages** who want familiar concepts mapped
   to Object Pascal and the Free Pascal libraries.

Every planned change should be measured against those readers.

## Focus and Scope

The cookbook is about **Free Pascal itself**: the language, compiler, Run-Time
Library (RTL), Free Component Library (FCL), and command-line ecosystem.

Lazarus appears when it genuinely helps, such as IDE and debugger workflows. LCL
GUI development and Lazarus-specific component catalogues are outside the main
scope. Third-party libraries may appear when no suitable FPC facility exists,
but those recipes must clearly identify the dependency, supported versions,
licence, and maintenance burden.

## Quality Goals

Every release applies the following goals to the pages it touches:

1. **Correctness** — facts, API names, signatures, version claims, and output are
   technically accurate.
2. **Verifiable examples** — complete examples compile in CI; selected
   deterministic examples also run in CI. Fragments and skips are reported
   honestly.
3. **Easy to understand** — prose uses plain English, manageable page sizes, and
   concrete examples suitable for beginners and ESL readers.
4. **Safe and idiomatic FPC** — examples demonstrate cleanup, input validation,
   bounds and overflow awareness, and protection against injection and memory
   errors.
5. **FPC-first** — examples use the compiler, RTL, and FCL directly unless a
   clearly explained dependency adds material value.

## Release and Compatibility Policy

Release numbers describe the maturity of this documentation project; they do not
promise a software-style public API.

- **0.9.x** releases may reorganise pages while the information architecture is
  being improved.
- **1.0.0** means the core learning path, verification baseline, and contribution
  process are trustworthy and documented. It does not mean every possible FPC
  topic is covered.
- **1.x** releases may add or reorganise material. Existing public URLs should be
  preserved where practical; moved pages should receive redirects rather than
  forcing a major release solely because navigation changed.

Compatibility statements must distinguish between:

- the **stable baseline** compiled in required CI;
- the **current Lazarus compatibility target** used only by relevant recipes;
- and **FPC trunk** checks, which are informative until a feature reaches a
  stable compiler release.

At the time this roadmap was revised, FPC 3.2.2 remained the stable baseline,
FPC trunk snapshots used version 3.3.1, and Lazarus 4.8 was the current Lazarus
compatibility target. These statements must be rechecked at release time.

---

## v0.9.3 — Trustworthy Validation

Make the validation report accurately describe what is and is not tested.

- [ ] **Correct snippet classification and totals.** Report complete programs,
      units, fragments, and explicit skips as separate, non-overlapping groups.
      Do not describe saved or skipped code as compile-tested.
- [ ] **Audit every `SKIP_COMPILE` marker.** Give every skip a reason and convert
      it to a safe, compilable example where practical.
- [ ] **Compile standalone units where possible.** A unit that can compile on its
      own should be validated rather than merely saved.
- [ ] **Add opt-in runtime validation.** Only programs marked as deterministic and
      safe to run are executed. Runtime metadata must support expected output,
      arguments, stdin, timeout, and supported platforms.
- [ ] **Check runtime failure modes.** Opted-in programs fail validation on a
      timeout, unexpected exit code, or unexpected stdout/stderr.
- [ ] **Pin documentation dependencies** and remove placeholder comments from
      `requirements.txt`.
- [ ] **Validate internal links and anchors** in pull requests. Internal-link
      failures are deterministic and should block a merge.

### v0.9.3 acceptance criteria

- [ ] The report totals reconcile and every category has an explicit meaning.
- [ ] Every complete program compiles on required Windows and Linux jobs.
- [ ] Every runtime-enabled example is isolated, time-limited, and deterministic.
- [ ] MkDocs strict build and internal-link validation pass in CI.

---

## v0.9.4 — Content and Information Architecture Hardening

Review each page once against a combined correctness, clarity, safety, and link
checklist. This avoids repeatedly touching the whole site in separate audit
releases.

- [ ] **Perform a page-by-page technical review.** Verify API names, signatures,
      return values, output, platform notes, and tested-version claims.
- [ ] **Review safe and idiomatic usage.** Check resource ownership,
      `try..finally`, input validation, bounds and overflow behaviour, SQL
      parameters, and `TProcess.Parameters` where relevant.
- [ ] **Simplify language while reviewing.** Shorten long sentences, explain
      unavoidable jargon, and prefer focused examples.
- [ ] **Split oversized pages before stabilising navigation.** Prioritise the
      Object Pascal introduction, NumLib examples, threading snippets, and file
      handling. Preserve useful old URLs with redirects when publishing moves.
- [ ] **Standardise admonitions and terminology.** Use consistent warning, tip,
      and note conventions. Maintain a curated glossary of terms readers are
      likely to need rather than attempting to define every technical word.
- [ ] **Verify encoding and platform claims** on Windows and Linux, especially
      string code pages, paths, files, processes, and console behaviour.
- [ ] **Create an explicit compatibility matrix.** Record required FPC 3.2.2
      results, relevant Lazarus results, and non-blocking FPC trunk results.
- [ ] **Record third-party provenance.** For vendored Synapse and ezthreads
      sources and `sqlite3.dll`, record the upstream repository or download,
      exact revision/version, licence, checksum where appropriate, and update
      procedure.
- [ ] **Run external-link health checks on a schedule.** Report redirects and
      failures for maintenance without making transient external outages block
      every pull request.

### v0.9.4 acceptance criteria

- [ ] Every published page has a recorded review result and reviewer/date.
- [ ] No page claims support for a version or platform that was not verified or
      clearly qualified.
- [ ] Oversized beginner-facing pages have a documented split or retention
      decision.
- [ ] Every vendored source or binary has reproducible provenance.

---

## v0.9.5 — Essential Coverage for 1.0

Fill the gaps needed for a complete core learning path. Existing material should
be extracted or strengthened instead of duplicated.

### Core language and collections

- [ ] **Focused records recipe.** Extract and improve the existing regular and
      advanced record material rather than writing a third introduction.
- [ ] **Sets.** Cover declaration, membership, ranges, and the `+`, `-`, and `*`
      operations.
- [ ] **Variants.** Explain use cases, conversions, runtime costs, and safer
      alternatives.
- [ ] **Sorting semantics.** Build on the existing array and list examples with
      comparer reuse, stable versus unstable ordering, and selection guidance.
- [ ] **Maps and owning containers.** Cover `TFPGMap`, `TDictionary`,
      `TFPObjectList`, and `TFPGObjectList`, including ownership behaviour.

### Streams, files, and tools

- [ ] **Streams explained.** Introduce `TStream`, `TMemoryStream`, and
      `TStringStream`, including position, size, ownership, and copying.
- [ ] **Text construction and writing.** Explain appropriate uses of string
      builders and stream writers without implying that they are always faster
      or preferable to ordinary strings.
- [ ] **Strengthen CSV coverage.** Extend the existing reading examples with
      quoting, malformed input, validation, and writing.
- [ ] **Command-line subcommands.** Parse commands and their arguments beyond the
      current single-option examples.
- [ ] **FPC toolchain.** Introduce `fpmake`, `fppkg`, `fpcres`, and `dynlibs` with
      small compiler-first examples. Mention Lazarus OPM only as an optional
      Lazarus ecosystem tool.

### Testing and learner orientation

- [ ] **Unit testing with FPCUnit.** Show a small production unit, a test case,
      a console runner, and failure output.
- [ ] **TDD-friendly project structure.** Show how source and tests can be kept
      separate and compiled from the command line.
- [ ] **What Free Pascal offers.** Provide a concise, evidence-based overview of
      native compilation, supported targets, language features, and standard
      libraries without promising universal static linking.
- [ ] **First cross-language guide.** Publish one complete guide, initially
      "Coming from Python", and use reader feedback before committing to the
      Java, C#, and C++ guides.

### v0.9.5 acceptance criteria

- [ ] Each new or extracted recipe has a runnable example or a justified reason
      why runtime execution is unsuitable.
- [ ] FPCUnit tests run in CI rather than only compiling.
- [ ] New navigation fits the reviewed information architecture.
- [ ] No recipe silently depends on Lazarus or an unidentified third-party unit.

---

## v1.0.0 — Trusted Core Release

Ship a stable core learning path without claiming that the entire FPC ecosystem
is complete.

- [ ] **Remove unfinished labels.** Resolve the remaining "work in progress"
      notices on published pages or remove those pages from navigation.
- [ ] **Document contribution standards.** Add `CONTRIBUTING.md` covering recipe
      structure, validation metadata, naming, safety expectations, and the
      review checklist.
- [ ] **Add community health files.** Add a code of conduct and focused issue and
      pull-request templates where they reduce contributor ambiguity.
- [ ] **Publish through GitHub Pages artifacts.** Build the site, upload a Pages
      artifact, and deploy it with the supported GitHub Pages actions. Do not
      require a generated `gh-pages` branch unless repository constraints make
      that necessary.
- [ ] **Document the permalink policy.** Preserve existing public URLs or add
      tested redirects for moved pages.
- [ ] **Publish a snippet coverage summary.** Show complete programs, compiled
      units, runtime-tested programs, fragments, and explicit skips as distinct
      values.

### Definition of done for 1.0

- [ ] Required CI passes on Windows and Linux.
- [ ] The core learning path contains no known correctness or safety defects.
- [ ] All published pages have completed the combined review checklist.
- [ ] Third-party sources and binaries have recorded provenance.
- [ ] A new contributor can validate a recipe using documented commands.
- [ ] The deployed site has no known broken internal links or unfinished pages.

---

## Post-1.0 Themes

These are candidate directions, not promised release contents or ordering.

### Data and persistence

- **PostgreSQL first:** parameterised queries, transactions, error handling, and
  connection lifecycle using FPC SQLDB. The recipe needs a reproducible database
  fixture before it is scheduled.
- **MySQL/MariaDB:** consider after the PostgreSQL recipe establishes a reusable
  database-recipe structure.
- **CSV and tabular follow-ups:** large files, streaming transformations, schema
  validation, and export.

### Networking with the RTL and FCL

- Raw TCP clients and servers using `Sockets`, with timeouts and bounds-checked
  buffers.
- UDP messaging and its delivery limitations.
- Streaming large downloads to disk with cancellation and cleanup.
- REST client patterns that build on the existing HTTP recipes.

WebSockets require either substantial protocol implementation or a third-party
library and therefore belong in the ecosystem backlog until a maintained,
well-scoped dependency is selected.

### Testing and correctness

- Table-driven and parameterised FPCUnit tests.
- Test doubles and controlled filesystem/network fixtures.
- Integration testing across Windows, Linux, and supported compiler versions.

### Advanced language and runtime topics

- Class operators and operator overloading.
- RTTI and custom serialisation beyond hand-written JSON/XML mapping.
- Internationalisation with resource strings, `gettext`, and `.po` files.
- C interoperability and external libraries.
- Generics and interfaces in depth.

### Safe and idiomatic deep-dives

- Memory management: reference counting, interfaces, owner objects, dangling
  pointers, and heap-leak reports.
- Pointer safety: legitimate uses, buffer boundaries, and ownership contracts.
- Secure input and output: untrusted input, SQL and command injection, file
  permissions, and secret handling.
- Packaging and distribution with `fpmake`, `fppkg`, and platform-appropriate
  binary packaging.

### Cross-language orientation

After the first guide is reviewed, consider separate guides for Java, C#, and
C++. Each guide is its own deliverable with examples and acceptance criteria;
they are not one combined checkbox.

### Ecosystem backlog

The following topics are valuable but require third-party dependencies or a
larger maintenance commitment. They should be scheduled only when a maintained
library, CI strategy, and reader demand are identified:

- WebSockets.
- OAuth 2.0 end-to-end flows.
- gRPC and message queues.
- Protobuf and Parquet.
- Cloud-provider SDKs.
- Coroutines or fibres.

LCL GUI design, visual components, and Lazarus-specific package development
remain out of scope for this FPC-first cookbook. If demand justifies them, they
belong in a sibling Lazarus-focused project.

---

## Ongoing Tooling and Automation

These tasks may be pulled into a release when they directly support its
acceptance criteria.

- [ ] **macOS validation.** Add a Homebrew FPC job after confirming that the
      required units used by the cookbook are available on the runner.
- [ ] **FPC trunk validation.** Compile examples that require FPC 3.3.1 features
      in a non-blocking trunk job, pinned to a known snapshot or source revision.
- [ ] **Dependency update automation.** Configure Dependabot for Python
      dependencies and GitHub Actions after initial versions are pinned.
- [ ] **Vendored dependency review.** Periodically compare bundled sources and
      binaries with recorded upstream revisions and security-relevant changes.
- [ ] **Local container environment, demand-driven.** Add one only if contributor
      feedback shows that it solves a recurring setup problem; it would validate
      Linux behaviour and would not replace Windows CI.

## Site and Community Backlog

- **Search tuning:** verify coverage and add useful synonyms after the page
  structure stabilises.
- **Print or PDF export:** investigate browser print styles and maintained MkDocs
  plugins before selecting an implementation.
- **Versioned documentation:** consider `mike` only when the project actively
  supports multiple materially different FPC documentation baselines.
- **Translations:** begin only after the English structure is stable and a named
  maintainer can keep each language current. Choose languages from reader demand,
  not in advance.
- **Community showcase:** add when there are enough reader projects and ongoing
  consent to maintain the page.

---

## How to Contribute

Choose an unchecked item or a narrowly scoped part of a post-1.0 theme and open
an issue before starting substantial work. A roadmap item should be broken into
a focused task with explicit acceptance criteria before implementation.

New or substantially revised recipes should:

1. Follow the reviewed section structure and beginner-friendly tone.
2. Include a complete example where practical and declare any runtime metadata.
3. Give fragments and skipped examples an explicit purpose and reason.
4. Be listed in `mkdocs.yml` navigation when published.
5. Model safe, idiomatic FPC and identify all non-FPC dependencies.
6. Build cleanly with `mkdocs build --strict` and pass snippet validation.
7. Record the tested compiler, operating systems, and any relevant Lazarus or
   third-party version.
