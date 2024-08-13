# Command Line Parameters

## How do I capture command line arguments?

Use [`ParamStr(n)`](https://www.freepascal.org/docs-html/rtl/system/paramstr.html) to get the n-th arguments.

Note, `ParamStr(0)` gives you the name of the program or location where the program is invoked.

While [`ParamCount`](https://www.freepascal.org/docs-html/rtl/system/paramcount.html) give you the number of arguments.

Here is an example.

```pascal
program CLSimple;

var
  i: integer;

begin
  WriteLn('Number of command line arguments: ', ParamCount);

    // Display all command line arguments
  for i := 0 to ParamCount do
    WriteLn('Argument ', i, ': ', ParamStr(i));
end.
```

When you compile and run the snippet above on a CLI followed by `a b c`, you will see the list of arguments given.

```bash
$ ./CLSimple.exe a b c
Number of command line arguments: 3
Argument 0: path-to-your-program/CLSimple.exe
Argument 1: a
Argument 2: b
Argument 3: c

```





## How can I capture short options?

Use [`getopt`](https://www.freepascal.org/daily/doc/rtl/getopts/getopt.html). See an example below.

1. In `uses` section add `getopt`. Line 17.
2. Create a short option string, in our example, `a:bcd`. Line 24.
3. Call `getopt(shortOpts)` in a loop
      - capture and action each option
      - deal with `?` and `:` (for invalid option and missing argument)
      - until it returns `EndOfOptions`. Line 36-55.

```pascal linenums="1" hl_lines="17 34 36-55"
program GetOptSimple;

// Example usage (git bash):

// $ ./GetOptSimple -a "Hello" -b -c -d
// $ ./GetOptSimple -a "Hello" -bc -d
// $ ./GetOptSimple -a "Hello" -bcd
// $ ./GetOptSimple -dcb -a "Hello"

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  getopts;

var
  c: char = DEFAULT(char);
  shortOpts: string;

begin

  // If the external variable opterr is True (which is the default),
  // getopt prints an error message.
  // Ref: https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html
  //      https://www.freepascal.org/daily/doc/rtl/getopts/getopt.html
  OptErr := False;

  // Defining valid short options
  // For example; -a requires an argument,
  //              -b, -c and -d don't.
  shortOpts := 'a:bcd';

  repeat
    c := getopt(shortOpts);
    case c of
      'a': WriteLn('Option a was set with value ', optarg);
      'b': WriteLn('Option b was set');
      'c': WriteLn('Option c was set');
      'd': WriteLn('Option d was set');
      '?', ':': begin
        // If getopt finds an option character in argv that was not included
        // in options, or a missing option argument,
        //    - it returns `?` and
        //    - sets the external variable `optopt` to the actual option character.
        // If the first character of options is a colon (‘:’),
        //    - then getopt returns ‘:’ instead of ‘?’ to indicate a missing option argument.
        // Refs
        // - https://www.freepascal.org/docs-html/rtl/getopts/index.html
        // - https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html
        if (optopt = 'a') then
          WriteLn('Error: Option ', optopt, ' needs an argument.')
        else
          WriteLn('Error: Unknown option: ', optopt);
      end;
    end; // case
  until c = EndOfOptions;

  // The reminder, checks for non-option arguments (if any) using optind
  if optind <= paramcount then
  begin
    Write('Non options : ');
    while optind <= paramcount do
    begin
      Write(ParamStr(optind), ' ');
      Inc(optind);
    end;
    WriteLn;
  end;

end.
```