# Packages

## What are Packages?

Basically, packages are add-ons you can install to add or modify functionalities of your programs or the Lazarus IDE.

There are various kinds of packages for Free Pascal & Lazarus IDE. The following is a summary from [https://wiki.freepascal.org/packages(disambiguation)](https://wiki.freepascal.org/packages(disambiguation))

> ***Packages*** have different meanings FPC & Lazarus projects.
> 
> Types of packages:
> 
> - [Lazarus Runtime Packages](https://wiki.freepascal.org/Lazarus_Packages) adds a dependency on a **runtime package** and Lazarus adds the files to the search path.
> - [Lazarus (IDE)Packages](https://wiki.freepascal.org/Lazarus_IDE_Packages) is bundled with IDE and can be installed via main menu: `Package -> Install/Uninstall Packages..`.
> - [Lazarus Designtime Packages](https://wiki.freepascal.org/Lazarus_Packages) register themselves in the IDE, e.g. **designtime** editor components for the object inspector. These may contain runtime-only units.

## Package Managers

- **Online Package Manager**(GUI) - [https://wiki.freepascal.org/Online_Package_Manager](https://wiki.freepascal.org/Online_Package_Manager)
- **fpcupdeluxe**(GUI) - [https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases)
- **LazarusPackageManager**(CLI) - [https://github.com/Warfley/LazarusPackageManager](https://github.com/Warfley/LazarusPackageManager)

## A lib I need is not in my fav package manager, what can I do?

1. Contact the package manager maintaners to add, or
2. Download the lib and get FPC (via Lazarus GUI) to include the location of your downloaded lib.

## FPC & Lazarus Packages

!!! Note

    I only listed packages that are relevant for my projects and works with Free Pascal and Lazarus IDE. 
    
    There are more packages (and visual components) out there than listed here. 
    
    - [https://github.com/Fr0sT-Brutal/awesome-pascal](https://github.com/Fr0sT-Brutal/awesome-pascal)
    - [https://wiki.freepascal.org/Components_and_Code_examples#Default_components_provided_by_Lazarus](https://wiki.freepascal.org/Components_and_Code_examples#Default_components_provided_by_Lazarus)
    - [https://wiki.freepascal.org/Package_List](https://wiki.freepascal.org/Package_List)
    - [https://wiki.freepascal.org/Category:Libraries](https://wiki.freepascal.org/Category:Libraries)
    - [https://packages.lazarus-ide.org](https://packages.lazarus-ide.org)
    - BeniBella's collection - [https://www.benibela.de/sources_en.html#top](https://www.benibela.de/sources_en.html#top)
    - Check the forums and discord channel too.


### [LGenerics by avk](https://github.com/avk959/LGenerics)

`#generics`

A Generics library with tons of goodies by [A.Koverdyaev(avk)](https://github.com/avk959).


### [ArrayHelper by Willi Commer](https://github.com/WilliCommer/ArrayHelper)

`#array-helper`

Delphi class helper for TArray with functions like Add, Delete, IndexOf, Map, etc. Includes examples and testing. Extends TArray with functions similar to TList or TStrings for easier memory management. Introduces TArrayRecord to use dynamic arrays like objects without the need for a final Free call.

### Currency type

`#finance`

Actually, [`Currency` is a data type](https://wiki.freepascal.org/Currency) in Free Pascal. 

### [Indy](https://github.com/IndySockets/Indy)

`#network`

Indy -- Internet Direct -- is a well-known internet component suite for Delphi, C++Builder, and Free Pascal providing both low-level support (TCP, UDP, raw sockets) and over a 120 higher level protocols (SMTP, POP3, NNT, HTTP, FTP) for building both client and server applications.

### [ZeosLib](https://sourceforge.net/projects/zeoslib/)

`#lazarus` `#database`

The ZeosLib is a set of database components for MySQL, MariaDB, PostgreSQL, Interbase, Firebird, MS SQL Server, SAP Adaptive Server Enterprise and Adaptive Server Anywhere (previously Sybase), Oracle and SQLite for Delphi, FreePascal/Lazarus and C++ Builder.

### [mORMot2](https://github.com/synopse/mORMot2)

`#database`

Synopse mORMot 2 is an Open Source Client-Server ORM SOA MVC framework for Delphi 7 up to Delphi 11 Alexandria and FPC 3.2/trunk, targeting Windows/Linux/BSD/MacOS for servers, and any platform for clients (including mobile or AJAX).

### [Fundamentals 5](https://github.com/fundamentalslib/fundamentals5)

`#utils`

Fundamentals 5 is a library for Delphi and Free Pascal. This library can help with the following tasks.

- String, DateTime and dynamic array routines
- Unicode routines
- Hash (e.g. SHA256, SHA512, SHA1, SHA256, MD5)
- Integer (e.g. Word128, Word256, Int128, Int256)
- Huge Word, Huge Integer
- Decimal (Decimal32, Decimal64, Decimal128, HugeDecimal and signed decimals)
- Random number generators
- Cryptographic (Symmetric: AES, DES, RC2, RC4; Asymmetric: RSA, Diffie-Hellman, Elliptic Curve)
- Data structures (array, dictionary and map classes)
- Mathematics (Rational number, complex number, vector, matrix, statistics)
- JSON parser
- Google protocol buffer parser, utilities and Pascal code generator
- Socket library (cross platform - Windows, Linux, OSX, iOS, Android)
- TLS Client
- TLS Server
- TCP Client
- TCP Server
- HTTP Client
- HTTP Server
- HTML Parser
- XML Parser


### [ATSynEdit](https://github.com/Alexey-T/ATSynEdit)

`#lazarus` `#visual-component`

**ATSynEdit** is Multi-line editor control for Lazarus, which was made by Alexey Torgashin as an alternative to SynEdit. It is not based on SynEdit, it has totally different structure.

### [Libraries by Benjamin Rosseaux (Bero1985)](https://github.com/BeRo1985)

A collection of useful libraries in Object Pascal by [Benjamin Rosseaux](https://www.rosseaux.net).

- **[PasMP](https://github.com/BeRo1985/pasmp)**: A parallel-processing library to enhance performance in multi-core processor environments.
- **[FLRE](https://github.com/BeRo1985/flre)**: FLRE (Fast Light Regular Expressions), a fast and efficient regular expression library implemented in Object Pascal, focusing on core features and optimized for speed.
- **[PasJSON](https://github.com/BeRo1985/pasjson)**: A JSON parser and writer for Pascal, vital for modern data interchange and web development.
- **[PUCU](https://github.com/BeRo1985/pucu)**: The Pascal UniCode Utils Library, enhancing Unicode support within Pascal projects.
- And others.

See the full list on [BeRo1985's Github page](https://github.com/BeRo1985?tab=repositories).


### Libraries by [Benito van der Zander (benibella)](https://github.com/benibela?tab=repositories)

- **[Internet Tools](https://github.com/benibela/internettools)**: This package provides standard conformant XPath 2.0, XQuery 1.0 and XPath/XQuery 3.0 interpreters with extensions for - among others - JSONiq, pattern matching, CSS and HTML; as well as functions to perform HTTP/S requests on Windows/Linux/MacOSX/Android, an XSLT-inspired webscraping language, and an auto update class. See the [internettools documentation](http://www.benibela.de/documentation/internettools/) or a [short overview](http://www.benibela.de/sources_en.html#internettools).

- **[BBUtils](https://github.com/benibela/bbutils)**: These units consist of important low-level functions missing in FreePascal. For example:

    - String and array view for efficient processing of slices of a string or array.
    - Various array functions (add, delete, search, prealloc, ...) and array lists.
    - Various string functions (pchar/ansistring, compare, split, search, convert html entities, ...). string builder for fast construction of strings.
    - Various date/time parsing/formatting functions, supporting years before 0 and after 65535, and timezones
    - Mathematical functions (gcd, primes, Bernoulli statistics, prime/euler-phi sieve...)
    - Stable sorting function for arbitrary sized data arrays
    - Automatical translation of with tr['marked strings'] and components
    - A Pascal template language which is "compiled" to Pascal (similar to the C preprocessor)
    - ...
    - Read the docs: [bbutils documentation](https://www.benibela.de/documentation/bbutils/bbutils.html)

- **[Big Decimal math](https://github.com/benibela/bigdecimalmath)**: Big Decimal Math
This unit provides an arbitrary precision BCD float number type. The usecase is performing a few arithmetic operations with the maximal possible accuracy and precision, e.g. calculating the sum of numbers from a text files, where the conversion from decimal input to binary floats would take more time than the calculation. It can be used like any numeric type and supports:

    - At least numbers between 10^-2147483647 to 10^2147483647 with 2147483647 decimal digit precision
    - All standard arithmetic and comparison operators
    - Rounding functions (floor, ceil, to-even, ..)
    - Some more advanced operations, e.g. power and sqrt
    - Accurate and precise binary float (single/double/extended) to BCD float and string conversion
    - ..
    - Read the docs: [bigdecimalmath documentation](http://www.benibela.de/sources_en.html#bigdecimalmath)
- 