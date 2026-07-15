# Strings

## What is a `String`?

An unqualified `String` does not have one fixed underlying type. Its meaning
depends on the compiler mode and string-related directives:

- With `{$H-}`, `String` is an alias for `ShortString`, whose payload is limited
  to 255 bytes.
- With `{$H+}` and Unicode strings disabled, `String` is an alias for the
  variable-length `AnsiString`.
- With `{$mode delphiunicode}` or `{$modeswitch unicodestrings}`, `String` is an
  alias for `UnicodeString`, which uses UTF-16.

An unqualified `AnsiString` uses the dynamic code page `CP_ACP`. Free Pascal
uses the program's `DefaultSystemCodePage` when conversions involving that code
page are required. Do not assume that every `String` has the same encoding on
every platform; use an explicitly encoded string type and convert at system
boundaries when the encoding matters.

   Refs:

   - [DelphiUnicode compiler mode](https://www.freepascal.org/docs-html/prog/progse75.html)
   - [UnicodeStrings mode switch](https://www.freepascal.org/docs-html/prog/progsu105.html)
   - [What is a `String`?](https://wiki.freepascal.org/String)
   - [https://forum.lazarus.freepascal.org/index.php?topic=58131.0](https://forum.lazarus.freepascal.org/index.php?topic=58131.0)


## Display UTF-8 on a console

```pascal linenums="1"
begin
  WriteLn('勤奋,勤勉になる,부지런하다!👍');
  WriteLn('Press Enter key to exit');
  ReadLn;
end.                                 
```

Alternatively, you can assign your UTF-8 test to a `string` variable.

```pascal linenums="1"
var
  s: string = '勤奋,勤勉になる,부지런하다!👍';

begin
  WriteLn(s);
end.                        
```

!!! Note for Windows Users

    If you see garbage characters on console;
    
    1. your console might not support code page 65001, or
    2. your windows does not support UTF on API level (only read/write file in UTF-8)
   
    See this [answer from StackOverflow](https://stackoverflow.com/a/57134096) on how to enable code page 65001 on your console.


!!! Warning
   
    The same [answer from StackOverflow](https://stackoverflow.com/a/57134096) also shows how to enable UTF-8 on Windows (system-wide). 
   
    **DO NOT MISS** the caveat section and comments in from that answer.
      
    ***Enabling UTF-8 system-wide on Windows is currently in beta and could lead to unintended system-wide side effects.***

   Refs:

   - [https://wiki.freepascal.org/FPC_Unicode_support#Code_pages](https://wiki.freepascal.org/FPC_Unicode_support#Code_pages)
   - [https://stackoverflow.com/a/57134096](https://stackoverflow.com/a/57134096)
   - [https://superuser.com/a/1435645](https://superuser.com/a/1435645)


## What is my program's default code page?

`DefaultSystemCodePage` is the code page that Free Pascal uses when it encounters
`CP_ACP`. It is not a reliable way to query the operating system's actual code
page because a program can change it.

On Windows, Free Pascal initializes it from the Windows ANSI code page. On Unix,
`cwstring` initializes Free Pascal's C-library-based string conversion support
from the process locale. It uses locale settings derived from `LC_ALL`,
`LC_CTYPE`, or `LANG` and sets `DefaultSystemCodePage`. Free Pascal recommends
placing `cwstring` among the first units in the program's `uses` clause:

```pascal linenums="1"
program ShowDefaultCodePage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cwstring,
  {$ENDIF}
  SysUtils;

begin
  WriteLn('Free Pascal default code page: ', DefaultSystemCodePage);
end.
```

A value of `65001` means UTF-8. A value of `0` is the `CP_ACP` identifier; it
means "use the program's default ANSI code page" rather than "the system uses
ASCII". The console's input/output encoding and font support are separate, so a
UTF-8 default code page does not by itself guarantee that every character will
display correctly.

See the Free Pascal documentation for
[`DefaultSystemCodePage`](https://www.freepascal.org/docs-html/rtl/system/defaultsystemcodepage.html)
and [`CP_ACP`](https://www.freepascal.org/docs-html/rtl/system/cp_acp.html).


## Remove trailing chars at the end of a string

!!! Contribution

    Gustavo 'Gus' Carreno, from the [Unofficial Free Pascal Discord server](Source: https://discord.com/channels/570025060312547359/896807098518732901/1218403991092985877), shared a neat trick to remove trailing characters by using `SetLength(str, length(str) - n);`.

Let's say you have a loop that append strings with trailing characters at the end.

One way to remove trailing characters is use a flag to inside the `for` loop. The logic would be: do not add commas or spaces if we are at the end of the loop.

A simpler way is to use `SetLength(str, length(str) - n_chars_to_remove);`.

See the example below. 

1. The `for` loop completes a sentence with a comma and a space at the end. Line 19-20.
2. The trick; `SetLength(line, length(line) - 2);` removes the last 2 chars from the end of the sentence. Line 29.

```pascal linenums="1" hl_lines="19 20 29"
program RemoveTrailingChars;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes;

var
  cities: array of string = ('Sydney', 'Melbourne', 'Surabaya', 'Malang');
  city, line: string;

begin

  // Building a sentence using the array of string.
  line := 'I have been to the following cities: ';
  for city in cities do
    line := line + city + ', ';

  // Now, line has a trailing comma and a space at the end.
  WriteLn(line);

  // Here is a trick to remove additional characters at the end of a string
  // by Gustavo 'Gus' Carreno,  2024-03-16 @ 14:42.
  // Simply use SetLength(str, length(str) - n_chars_to_remove);
  // Source: https://discord.com/channels/570025060312547359/896807098518732901/1218403991092985877
  SetLength(line, length(line) - 2);

  // Finally, just add a dot.
  line := line + '.';
  WriteLn(line);

  // Pause console
  ReadLn;
end.
```
