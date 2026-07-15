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

Use [`GetOpt`](https://www.freepascal.org/docs-html/current/rtl/getopts/getopt.html)
from the `GetOpts` unit:

1. Define the accepted one-character options in a string.
2. Add `:` after an option that requires an argument.
3. Call `GetOpt` until it returns `EndOfOptions`.
4. Read an option's argument from `OptArg` and the first non-option argument
   from `OptInd`.

The leading `:` in `ShortOptions` makes `GetOpt` return `:` for a missing
argument and `?` for an unknown option.

```pascal linenums="1"
program GetOptSimple;

// Example: ./GetOptSimple -a "Hello" -bcd input.txt

{$mode objfpc}{$H+}{$J-}

uses
  GetOpts;

const
  ShortOptions = ':a:bcd';

var
  OptionChar: Char;

begin
  OptErr := False;

  repeat
    OptionChar := GetOpt(ShortOptions);
    case OptionChar of
      'a': WriteLn('Option a was set with value ', OptArg);
      'b': WriteLn('Option b was set');
      'c': WriteLn('Option c was set');
      'd': WriteLn('Option d was set');
      '?': WriteLn('Error: Unknown option: ', OptOpt);
      ':': WriteLn('Error: Option ', OptOpt, ' needs an argument.');
    end;
  until OptionChar = EndOfOptions;

  if OptInd <= ParamCount then
  begin
    Write('Non-option arguments: ');
    while OptInd <= ParamCount do
    begin
      Write(ParamStr(OptInd), ' ');
      Inc(OptInd);
    end;
    WriteLn;
  end;
end.
```

## How can I capture long options?

Use [`GetLongOpts`](https://www.freepascal.org/docs-html/current/rtl/getopts/getlongopts.html)
with an array of `TOption` records. Each record specifies:

- `Name`: the option name without the leading `--`.
- `Has_Arg`: `No_Argument`, `Required_Argument`, or `Optional_Argument`.
- `Flag`: when `nil`, `GetLongOpts` returns `Value`.
- `Value`: the character used to handle the option. Reusing the short-option
  character lets `--add` and `-a` share one branch.

The array must end with an entry whose `Name` is empty. `GetLongOpts` receives a
pointer rather than the array's length, so it uses this entry as the terminator.
`LongOptionIndex` receives the one-based position of the matching long option.

This calculator accepts either long or short operation names:

```text
./GetOptLongCalculator --add 12.5 7.5
./GetOptLongCalculator -m 6 7
```

```pascal linenums="1"
program GetOptLongCalculator;

{$mode objfpc}{$H+}{$J-}

uses
  GetOpts,
  SysUtils;

type
  TOperation = (opNone, opAdd, opSubtract, opMultiply, opDivide);

const
  LongOptions: array[1..6] of TOption = (
    (Name: 'add';      Has_Arg: No_Argument; Flag: nil; Value: 'a'),
    (Name: 'subtract'; Has_Arg: No_Argument; Flag: nil; Value: 's'),
    (Name: 'multiply'; Has_Arg: No_Argument; Flag: nil; Value: 'm'),
    (Name: 'divide';   Has_Arg: No_Argument; Flag: nil; Value: 'd'),
    (Name: 'help';     Has_Arg: No_Argument; Flag: nil; Value: 'h'),
    (Name: '';         Has_Arg: No_Argument; Flag: nil; Value: #0)
  );

var
  Operation: TOperation;
  OptionChar: Char;
  LongOptionIndex: LongInt;
  LeftValue, RightValue, ResultValue: Double;

procedure ShowUsage;
begin
  WriteLn('Usage: ', ExtractFileName(ParamStr(0)),
    ' OPTION NUMBER NUMBER');
  WriteLn('Options:');
  WriteLn('  -a, --add');
  WriteLn('  -s, --subtract');
  WriteLn('  -m, --multiply');
  WriteLn('  -d, --divide');
  WriteLn('  -h, --help');
end;

procedure Fail(const MessageText: String);
begin
  WriteLn('Error: ', MessageText);
  ShowUsage;
  Halt(1);
end;

procedure SelectOperation(const NewOperation: TOperation);
begin
  if Operation <> opNone then
    Fail('Choose exactly one operation.');
  Operation := NewOperation;
end;

begin
  Operation := opNone;
  OptErr := False;

  repeat
    OptionChar := GetLongOpts('asmdh', @LongOptions[1], LongOptionIndex);
    case OptionChar of
      'a': SelectOperation(opAdd);
      's': SelectOperation(opSubtract);
      'm': SelectOperation(opMultiply);
      'd': SelectOperation(opDivide);
      'h':
        begin
          ShowUsage;
          Halt(0);
        end;
      '?', ':': Fail('Unknown or malformed option.');
    end;
  until OptionChar = EndOfOptions;

  if Operation = opNone then
    Fail('Choose an operation.');

  if ParamCount - OptInd + 1 <> 2 then
    Fail('Provide exactly two numbers.');

  if not TryStrToFloat(ParamStr(OptInd), LeftValue) then
    Fail('The first operand is not a number.');
  Inc(OptInd);
  if not TryStrToFloat(ParamStr(OptInd), RightValue) then
    Fail('The second operand is not a number.');

  case Operation of
    opAdd: ResultValue := LeftValue + RightValue;
    opSubtract: ResultValue := LeftValue - RightValue;
    opMultiply: ResultValue := LeftValue * RightValue;
    opDivide:
      begin
        if RightValue = 0 then
          Fail('Cannot divide by zero.');
        ResultValue := LeftValue / RightValue;
      end;
    else
      ResultValue := 0;
  end;

  WriteLn('Result: ', ResultValue:0:2);
end.
```

!!! contribution

    This example was inspired by
    [zargex's `dumb_calculator`](https://github.com/zargex/dumb_calculator),
    shared in [issue #6](https://github.com/ikelaiah/free-pascal-cookbook/issues/6).
    The cookbook version adds a terminating option entry, named argument
    constants, validation, and error handling.
