# Regex

## Match a filename using regex

Here is an example of using `TRegExpr` and `\w*.txt$` to match `*.txt` files.

1. In `uses` section add `RegExpr`. Line 11.
2. Create the `TRegExpr` object Line 20.
3. Enable case insensitive flag. Line 23. 
4. Apply the regex expression. Line 25.
5. Use `TRegExpr.Exec` to find a match in the input string. Line 27.
6. `Free` the `TRegExpr` object at the end. Line 33.

```pascal linenums="1" hl_lines="11 20 23 25 27 33"
program MatchingFilename;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  RegExpr;

var
  regex: TRegExpr;
  regexPattern: string = '\w*.txt$';
  filename: string = 'hello-text.txt';

begin
  // Create TRegExpr
  regex := TRegExpr.Create;
  try
    // Set the regex to case-insensitive
    regex.ModifierI := True;
    // Apply the regex pattern
    regex.Expression := regexPattern;
    // Check for a match
    if regex.Exec(filename) then
      WriteLn(Format('''%s'' matches %s!', [regexPattern, filename]))
    else
      WriteLn(Format('''%s'' does not match %s!', [regexPattern, filename]));
  finally
    // Free TRegExpr
    regex.Free;
  end;

  // Pause console
  WriteLn('Press Enter key to exit ...');
  ReadLn;
end.
```
For more info check [https://regex.sorokin.engineer/en/latest/tregexpr.html](https://regex.sorokin.engineer/en/latest/tregexpr.html).

## Match a filename using regex (Alt)

If you find the previous snippet to be untidy, you have the option to encapsulate the regular expression matching within a function. See the snippet below, line 13-33.

See the snippet below, matching a filename using a regex is now a one liner. Line 40.

```pascal linenums="1" hl_lines="13-33 40"
program MatchingFilenameAlt;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  RegExpr;

  // A function for matching filename against a regex pattern
  function IsFileNameMatching(const fileName: string; const regexPattern: string): boolean;
  var
    regex: TRegExpr;
  begin
    regex := TRegExpr.Create;
    try
      // Set the regex to case-insensitive
      regex.ModifierI := True;
      // Apply the regex pattern
      regex.Expression := regexPattern;
      // Check for a match
      if regex.Exec(filename) then
        Result := True
      else
        Result := False;
    finally
      // Free TRegExpr
      regex.Free;
    end;
  end;

var
  regexPattern: string = '\w*.txt$';
  filename: string = 'hello-text.txt';

begin
  if IsFileNameMatching(filename, regexPattern) then
    WriteLn(Format('%s matches %s!', [regexPattern, filename]))
  else
    WriteLn(Format('%s does not match %s!', [regexPattern, filename]));

  //Pause console
  WriteLn('Press Enter kay to exit ...');
  ReadLn;
end.
```

## Match a string using regex and print out all matches

Here is an example of using `TRegExpr` to find and print out matches, where the user specify the regex as a command line argument.

The program will keep on asking for an input string until a match is found.

1. In `uses` section add `RegExpr`. Line 21.
2. Create the `TRegExpr` object using `ParamStr(1)` as the expression to evaluate. Line 33.
3. Enable case insensitive flag. Line 36. 
4. Use `Exec` to find a match in the input string. Line 43.
5. If input is found use while loop to get the values in `re.Matches[i]`. Line 48-53.
6. `Free` the `TRegExpr` object at the end. Line 60.

```pascal linenums="1" hl_lines="21 33 36 43 48-53 60"
program RegexExample;

// Program will quit when you give a text input matching regex pattern
// in the program's argument.

// For example.
// $ RegexExample.exe "(\d{1,4})[-/.](\d{1,2}|[a-zA-Z]{3,})[-/.](\d{1,4})"

// Enter a text:24-Mar-2024
// Matches!
// Note! Match[0] is the entire match!
// Match 0 : 24-Mar-2024
// Match 1 : 24
// Match 2 : Mar
// Match 3 : 2024
// $

{$mode objfpc}{$H+}{$J-}

uses
  RegExpr;

var
  re: TRegExpr;
  input: string;
  i: integer;
begin

  // If user input is '', exit program
  if ParamStr(1) = '' then Exit;

  // Create the regex object using first argument of the program
  re := TRegExpr.Create(ParamStr(1));
  try
    // Set regex flag to case-insensitive
    re.ModifierI := True;
    // Keep on asking the user until there is a match
    repeat
      WriteLn;
      Write('Enter a text:');
      ReadLn(input);
      // If there is a match, and match[0] (global) is not '', show all matches
      if re.Exec(input) and (re.Match[0] <> '') then
      begin
        WriteLn('Matches!');
        WriteLn('Note! Match[0] is the entire match!');
        // Loop through matches using re.Matches[i]
        i := 0;
        while re.Match[i] <> '' do
        begin
          WriteLn('Match ', i, ' : ', re.Match[i]);
          Inc(i);
        end;
      end
      else
        WriteLn('No match, try again.');
    until re.Match[0] <> '';
  finally
    // Free TRegExpr object
    re.Free;
  end;
end.
```

You can find the latest doc of TRegExpr here: [https://regex.sorokin.engineer/en/latest/](https://regex.sorokin.engineer/en/latest/).


## How can I replace date separators with question marks?

See the example below. The algorithm is encapsulated in `ReplaceDateSeparatorWithQMark`.

1. Create an instance of TRegExpr. Line 17.
2. Create a regex to capture date, month and year in common formats; `d`, `dd`, `m`, `mm`, `mmm`, `yy` and `yyyy` format, in any order. Line 20. 
3. Use `TRegExpr.Replace` on a date string to capture potential (1)date, (2)month and (3)year groups, and put `?` between them. Also, return the result to function caller. Line 26.
4. `Free` the `TRegExpr` object. Line 28.


```pascal linenums="1" hl_lines="17 20 26 28"
program ReplaceDateSeparators;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  RegExpr,
  SysUtils;

  function ReplaceDateSeparatorWithQMark(dateString: string): string;
  var
    re: TRegExpr;
  begin
    re := TRegExpr.Create;
    try
      // A regex to capture common date formats: dd.mm.yyyy, yyyy.mm.dd, dd.mmm.yy
      re.Expression := '(\d{1,4})[-/.](\d{1,2}|[a-zA-Z]{3,})[-/.](\d{1,4})';

      // The next line does 3 tasks.
      // 1. Capture date, month and year groups
      // 2. Construct a date string and using `?` as separators
      // 3. And return the result to function caller
      Result := re.Replace(dateString, '$1?$2?$3', True);
    finally
      re.Free;
    end;
  end;

var
  dateInput1: string = '24-Mar-24';
  dateInput2: string = '24/03/24';
  dateInput3: string = '2024/03/24';

begin
  try
    WriteLn(ReplaceDateSeparatorWithQMark(dateInput1));
    WriteLn(ReplaceDateSeparatorWithQMark(dateInput2));
    WriteLn(ReplaceDateSeparatorWithQMark(dateInput3));
  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;
  // Pause Console
  ReadLn;

end.
```

If you run the snippet above, the output would be as follows.

```text
24?Mar?24
24?03?24
2024?03?24
```

### Why would you convert date separators to question marks?

1. I needed it as a Regex practice.
2. I need it when I work with [`ScanDateTime`](https://www.freepascal.org/docs-html/rtl/dateutils/scandatetime.html), as `?` will match any character in the input string.

## How do I use TRegExpr in a GUI application?

Michaël Van Canneyt has written a concise example: [Using Regular Explession](https://www.freepascal.org/~michael/articles/regex/regex.pdf).

