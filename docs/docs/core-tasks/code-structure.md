# Structuring Your Codes

## Structure of a Pascal Program

Here is a common structure of a Pascal program.

```pascal linenums="1"
program ProgramStructure;

// Global compiler directives here
// Ref: https://www.freepascal.org/docs-html/prog/progse3.html

uses
  // Add units here

const
  // Add consts here
  // Refs: https://www.freepascal.org/docs-html/ref/refse9.html#x21-200002.1
  //       https://www.freepascal.org/docs-html/ref/refse10.html#x22-210002.2

resourcestring
  // Declare resourcestrings
  // Ref: https://www.freepascal.org/docs-html/ref/refse11.html

type
  // Declare types

var
  // Declare variables, initialise when possible

threadvar
  // Declare threadvars
  // Variables in this section have unique values for each thread
  // Ref: https://www.freepascal.org/docs-html/ref/refse26.html

  // Define procedures and functions before the MAIN entry/block of the program

begin
   // This is the MAIN entry/block
end.         
```

## An example of a Pascal Program

Here is a program example that stores student information in a record, and print it on the console.

```pascal linenums="1"
program SimpleProgram;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils;

const
  student_id_prefix: string = 'ua-';

type
  TStudent = record
    studentId: string;
    firstname: string;
    lastname: string;
  end;

  // Prints the contents of a TStudent var
  procedure PrintStudentInfo(student: TStudent);
  begin
    WriteLn(student.studentId);
    WriteLn(student.firstname, ' ', student.lastname);
  end;

var
  myStudent: TStudent;

begin
  // The Main block/entry of the program

  WriteLn('Now : ', DateToStr(Now));

  myStudent.firstname := 'John';
  myStudent.lastname := 'Costco';
  myStudent.studentId := student_id_prefix + '2227209';
  PrintStudentInfo(myStudent);

  // Pause console
  WriteLn('Press Enter key to quit ...');
  ReadLn();
end.
```

The output as follows.

```text
Now : 16/12/2023
ua-2227209
John Costco
Press Enter key to quit ...
```

## Structure a Unit

```pascal linenums="1"
program UnitStructure;

interface

  // This is the Public section.
  // Variables, functions and procedures declared in this section
  // will be accessible from the unit's caller.

implementation

  // This is the Private section.
  // Anything declared in this section will only be available to the unit.

initialization

  // Optional. Code that runs when the unit gets loaded.
  // Ref: https://www.freepascal.org/docs-html/ref/refse112.html#x233-25700016.2

finalization

  // Optional. Code that runs when the program ends normally.
  // The finalization part of the units are executed in the
  // reverse order of the initialization execution.
  // Ref: https://www.freepascal.org/docs-html/ref/refse112.html#x233-25700016.2

end.
```

## An Example of a Unit

Here is an example of a simple unit for calculating the areas of a square and a circle.

1. the unit has a private variable called `short_pi`, which is not available outside the unit itself.
2. the unit has two public functions.

```pascal linenums="1"
unit Areas;

interface

function CalcAreaSquare(side: real): real;
function CalcAreaCircle(radius: real): real;

implementation

const
  shortPI: real = 3.14;

function CalcAreaSquare(side: real): real;
begin
  Result := side * side;
end;

function CalcAreaCircle(radius: real): real;
begin
  Result := shortPI * radius * radius;
end;

end.
```

We can use this `Areas` unit as follows.

```pascal linenums="1" hl_lines="10 15 20"
program SimpleProgramWithUnit;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  Areas;

begin
  // Calculate area of a square
  WriteLn('Area of 2.5cm square is ',
          CalcAreaSquare(2.5): 0: 2,
          ' cm².');

  // Calculate area of a circle
  WriteLn('Area of a circle with r=2.5cm is ',
          CalcAreaCircle(2.5):0: 2,
          ' cm².');

  // The following WriteLn will not compile
  // Because shortPI is declared in the private section of the Area unit
  // WriteLn('shortPI is ', Areas.shortPI);

  // Pause console
  WriteLn('Press Enter key to exit ...');
  ReadLn;
end.
```

!!! Contribution

    2024-02-08 - Gustavo 'Gus' Carreno 🇵🇹 pointed out on the original code; it is not necessary to include the unit's name when calling a routine.

    Previously we had `CalcAreaCircle(2.5)` as `Areas.CalcAreaCircle(2.5)`.
    
    Anything in the `interface` section of a Unit will become global for the scope of the program/unit/dll that includes it. Hence, the code above is only `CalcAreaCircle(2.5)`.

    ==We only need to include the name of the unit if a naming conflict occurs==.