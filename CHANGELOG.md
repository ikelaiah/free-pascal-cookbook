# Changelog

All notable changes to the Free Pascal Cookbook will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.9] - 2025-11-30

### Added

- sqlite-databases.md: Complete guide to SQLite database operations (CRUD - Create, Read, Update, Delete)
- http-requests-advanced.md: Making POST, PUT, and DELETE requests with JSON data and handling response status codes
- working-with-xml.md: Parsing XML files, creating XML programmatically, and comparing with JSON format
- system-calls.md: Running external programs, capturing command output, cross-platform execution, and handling exit codes
- sending-emails.md: SMTP email functionality including plain text, HTML emails, multiple recipients, and Gmail setup with app passwords
- api-authentication.md: Different authentication methods (API keys, Bearer tokens, Basic auth, OAuth 2.0) with error handling and security best practices
- environment-variables-config.md: Working with environment variables, .env files, INI configuration files, and a configuration helper class

## [0.8] - 2025-11-30

### Added

- Prerequisite knowledge note in intro-objpas-fpc.md to set expectations for beginners
- "Why use this?" explanations before advanced topics (Generics, Interfaces, Pointers)
- Comprehensive glossary section covering basic programming, OOP, advanced, and Free Pascal-specific terms
- Glossary entries include: Variable, Data Type, Constant, Function, Procedure, Class, Object/Instance, Constructor, Destructor, Property, Method, Inheritance, Encapsulation, Access Modifiers, Interface, Generic, Pointer, Memory Address, Reference Counting, Anonymous Function, Mode, Compiler Directive, Unit, GUID, Dynamic Array, Static Array, Enum, Subrange Type, Record, and Modeswitch
- Note in basic-hello-world.md explaining the `.pas` file extension
- macOS CLI compilation instructions in basic-hello-world.md
- Examples of text editors in basic-hello-world.md (Notepad, VS Code, Sublime Text)
- "What You'll Find Here" section in about.md describing cookbook contents
- "Acknowledgments & Attribution" section in about.md with clear feedback path via GitHub Issues
- "Staying Updated" section in about.md noting regular maintenance
- "Contributing" section in about.md with link to GitHub repository for open source contributions
- "What's Inside" section in README.md listing major topics covered
- "Who Is This For" section in README.md identifying target audiences
- "Featured Topics" section in README.md with links to beginner-friendly starting points
- Improved "Getting Started" section in README.md with clearer navigation guidance
- Better "Installation" explanation in README.md clarifying that no installation is required for the cookbook itself but FPC/Lazarus are optional for using the language
- "Getting Help" section in README.md with links to community resources and support
- "Contributing" section in README.md inviting improvements and bug reports via GitHub
- Version badges in README.md header showing version 0.8, MIT license, Free Pascal, and Lazarus IDE

### Fixed

- Formatting issue in section 11 header (changed "## 11.Math Operations" to "## 11. Math Operations")
- Markdown linting issues (blank lines around lists, trailing newline)
- Grammar in basic-hello-world.md ("the `fpc` is" changed to "`fpc` is")
- Improved clarity of text editor launch instructions in basic-hello-world.md
- common-data-types.md now written for 15-18 year old students with clear, accessible language
- Added "Understanding Data Types" intro section explaining why different types exist
- Added "Quick Reference: Which Type Should I Use?" table with practical guidance
- Added "When to use" sections for all major data types (Byte, ShortInt, Integer, Int64, Single, Double, String, Enum, Subrange)
- Clarified technical terms: "accurate decimal places" (instead of "significant digits"), "unsigned" (no negatives), "platform-dependent"
- Fixed Extended type example (changed invalid exponent 4932 to 308)
- Updated Regular Records and Advanced Records to use modern dynamic `String` instead of `ShortString`
- Added required `{$modeswitch advancedrecords}` to Advanced Records example
- Added warning box before Pointers section cautioning beginners
- Improved String type explanation to contrast modern vs legacy ShortString
- Added note about floating-point precision limitations and practical implications
- Explained Enumerated and Subrange types with real-world use cases
- Improved about.md grammar and clarity (changed "could be challenging" to "I found them challenging")
- Enhanced about.md with clear call-to-action for feedback via GitHub Issues
- Added information about cookbook maintenance and updates
- Made about.md more informative for visitors about what to expect on the site


## [0.7] - 2025-10-15

Initial release with core documentation covering:

- Basic Free Pascal getting started guides
- Data types reference
- Object-oriented programming introduction
- Example programs and tutorials