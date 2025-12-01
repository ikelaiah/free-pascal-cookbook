# Intro to Object Pascal using FPC

This page is my go-to guide to the key basics of Object Pascal with the [Free Pascal Compiler](https://www.freepascal.org). It's not a complete guide, but it's got all the essentials to help you dive in and discover just how powerful Free Pascal can be!

Hope you find it helpful! 🚀

!!! Note

    **Prerequisite Knowledge**: This guide assumes you have a basic understanding of programming concepts such as variables, data types, and control flow. If you're completely new to programming, you may want to start with a general programming tutorial first.

!!! Note

    Setup the Lazarus IDE (which includes the Free Pascal Compiler) for your OS to run the snippets on this page.

---

## 1. Hello, World! 

Here is a simple program that prints `Hello, World!` in Free Pascal.

```pascal linenums="1"
program HelloWorldSimple;
{$mode objfpc}{$H+}{$J-}

begin
  WriteLn('Hello, World!');
  // On some systems, you might need ReadLn here to see the output
  // before the console window closes.
end.
```

!!! tip "Tip: Running from the Lazarus IDE"
    When running a console program in the Lazarus IDE, the console window may close too quickly to see the output. 
    
    Use `ReadLn` to keep it open until you press the ++enter++ key.

    ```pascal linenums="1" hl_lines="5"
    program HelloWorldWithPause;
    {$mode objfpc}{$H+}{$J-}

    begin
      WriteLn('Hello, World!');
      ReadLn; // Waits for the user to press Enter
    end.
    ```

## 2. Reserved Words

Reserved words are special words in the Pascal language that you cannot change or redefine.

!!! Note

    The Free Pascal Compiler lets you use uppercase or lowercase letters for these special words; they will work the same way.

The following keywords exist in Turbo Pascal mode.

```text
absolute    file            object      string  
and         for             of          then  
array       function        operator    to  
asm         goto            or          type  
begin       if              packed      unit  
case        implementation  procedure   until  
const       in              program     uses  
constructor inherited       record      var  
destructor  inline          reintroduce while   
div         interface       repeat      with  
do          label           self        xor
downto      mod             set         
else        nil             shl         
end         not             shr         
```

The special words in Object Pascal (used in Delphi or Objfpc mode) are the same as in Turbo Pascal, but with these extra keywords:

```text
as              finalization    library     raise   
class           finally         on          resourcestring     
dispinterface   initialization  out         threadvar  
except          inline          packed      try 
exports         is              property    
```

## 3. Comments

Comments are pieces of the source code which are completely discarded by the compiler.

In Free Pascal, you can create comments by using the following methods:

1. `(*` and `*)`
2. `{` and `}`
3. `//`

!!! Tip
    
    [Mobius1 — 2024-10-06 at 08:15](https://discord.com/channels/570025060312547359/570025060312547361/1292233646928957524)

    Suggested the addition of `(* ... *)`. Since `{ ... }` can be combined with directives, it may confuse the syntax checker.

    You can use `(* ... *)` to prevent accidental `{ }`.

    ```pascal
    (* 
       Example of multiple lines commented.
       And immune to accidental }
    *)
    ```

**Examples**

```pascal linenums="1"
(* 
 Example of multiple lines commented.
 And immune to accidental }
*)

{  This is a single line comment. }  

// This is a single line comment. All is ignored till the end of the line.

{
  This is a multi-line comment in Object Pascal.
  You can write as much text as you want here,
  and it will all be ignored by the compiler.
  Multi-line comments are useful for explaining
  more complex parts of your code or adding
  detailed documentation.
}
```

## 4. Main Block

An Object Pascal program must have a main (program) block, marked by `begin ... end.`. Note the `.` after the `end`.
All programs should also start with a `program ProgramName;` declaration.

```pascal linenums="1"
program MyFirstProgram;
{$mode objfpc}{$H+}{$J-} // Basic compiler directives

{ This is the main block }
begin
  { ... your code ... }
  
  // Example, print a message.
  WriteLn('Hello, World from MyFirstProgram!');
  ReadLn; // Pause to see output
end.
```

## 5. Variable and Basic Types

### Declaration

Declare variables within the `var` section of your program, between the `program` line (and any `uses` clause) and the main `begin`.

**Syntax**

```pascal linenums="1"
var
  variableName:dataType; 
```

**Example**

Here's a simple example of declaring basic variable types in Free Pascal.

```pascal linenums="1"
program VariableDeclarationExample;
{$mode objfpc}{$H+}{$J-}

var
  myChar: char;       // A single character, like 'A' or 'b'
  myString: string;   // A sequence of characters, like "Hello, world!"
  myInt: integer;     // A whole number, like 42 or -7
  myBool: boolean;    // A true or false value
  myReal: real;       // A number with decimals, like 3.14 or -0.5
begin
  // Variables are declared, but not yet initialized with specific values here.
  // We'll assign values in the next example.
  WriteLn('Variables declared. Program will now end.');
  ReadLn;
end.
```

Check out the official documentation [Types](https://www.freepascal.org/docs-html/ref/refch3.html#refse12.html) for a full list and explanation of the types you can use.

### Assignment

!!! Tip
    After declaring a variable, make sure to initialise it before use, else you might end up with a garbage value.

!!! Note
    `:=` is assignment.

    `=`  is comparison, equality.

Use `:=` for assigning a variable to a value.

**Syntax**

```pascal
variableName := value
```

**Example**

```pascal linenums="1" hl_lines="14-18"
program BasicVariableTypes;

{$mode objfpc}{$H+}{$J-}

var
  myChar: char;       // A single character, like 'A' or 'b'
  myString: string;   // A sequence of characters, like "Hello, world!"
  myInt: integer;     // A whole number, like 42 or -7
  myBool: boolean;    // A true or false value
  myReal: real;       // A number with decimals, like 3.14 or -0.5

begin
  // Assign values to the variables
  myChar := 'A';
  myString := 'Hello, World!';
  myInt := 42;
  myBool := True;
  myReal := 1234.5678;

  // Print the values of the variables to the console
  Writeln('Character: ', myChar);
  Writeln('String: ', myString);
  Writeln('Integer: ', myInt);
  Writeln('Boolean: ', myBool);
  Writeln('Real: ', myReal:0:4);

  // Pause Console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

## 6. Console 

### User Input

In Free Pascal, `Read` and `ReadLn` are used for input, but they work a bit differently.

Use `Read` when you want to read input without moving to the next line.

```pascal linenums="1"
program ReadExample;
{$mode objfpc}{$H+}{$J-}

var
  num1, num2: integer;
begin
  { Using Read -- The next value read will be num 2. }
  Write('Enter two numbers separated by a space: ');
  Read(num1);
  Read(num2);
  WriteLn; // Move to next line for cleaner output
  WriteLn('You entered: ', num1, ' and ', num2);
  ReadLn; // Pause before exit
  ReadLn; // Second ReadLn to consume the Enter from the number input
end.
```

Use `ReadLn` when you want to read input and move to the next line afterward.

```pascal linenums="1"
program ReadLnExample;
{$mode objfpc}{$H+}{$J-}

var
  num1, num2: integer;
begin
  { Using ReadLn -- The next value after a new line will be num 2. }
  WriteLn('Enter first number: ');
  ReadLn(num1);
  WriteLn('Enter second number: ');
  ReadLn(num2);
  WriteLn('You entered: ', num1, ' and ', num2);
  ReadLn; // Pause before exit
end.
```

### Display Text

Similarly, `Write` and `WriteLn` are used to output text, but they behave differently.

Use `Write` to output text without moving to the next line. It keeps the cursor on the same line, so subsequent output will continue from where the previous output ended.

```pascal linenums="1"
program WriteExample;
{$mode objfpc}{$H+}{$J-}

begin
  { Using Write -- World! appears after Hello. }
  Write('Hello '); // No new line at the end of the string.
  Write('World!'); // No new line at the end of the string.

  // A spacer
  WriteLn;
  ReadLn; // Pause
end.
```

Use `WriteLn` to output text and then moves the cursor to the next line. It adds a newline character after the text, so any subsequent output starts on a new line.


```pascal linenums="1"
program WriteLnExample;
{$mode objfpc}{$H+}{$J-}

begin
  { Using WriteLn -- World! appears underneath Hello. }
  WriteLn('Hello '); // There is a new line at the end of the string.
  WriteLn('World!'); // There is a new line at the end of the string.
  ReadLn; // Pause
end.
```

## 7. Routines

### Procedure

A `procedure` is a block of code that performs a specific task but does not return a value. Use it when you want to execute a series of statements without returning a result.

**Syntax**

```pascal linenums="1"
procedure ProcedureName(AParam:Type;...);
const
  { const section }
var
  { var section }
begin
  { your code goes here }
end;
```

**Example - Procedure without parameters**

```pascal linenums="1" hl_lines="4-7"
program ExampleProcedureWithoutParams;
{$mode objfpc}{$H+}{$J-}

procedure Greet;
begin
  WriteLn('Hello, World!');
end;

begin
  Greet;  // Calling the procedure
  ReadLn;
end.
```

**Example - Procedure with Parameters**

```pascal linenums="1" hl_lines="4-7"
program ExampleProcedureWithParams;
{$mode objfpc}{$H+}{$J-}

procedure Greet(name: string);
begin
  WriteLn('Hello, ', name, '!');
end;

begin
  Greet('Alice'); // Calling the procedure with arguments
  ReadLn;
end.
```

**Example - Procedure with `var` section**

```pascal linenums="1" hl_lines="4-11"
program ExampleProcedureWithVarSection;
{$mode objfpc}{$H+}{$J-}

procedure SwapNumbers(var x, y: integer);
var
  temp: integer;  // Local variable to help with swapping
begin
  temp := x;  // Store the value of x in temp
  x := y;     // Assign the value of y to x
  y := temp;  // Assign the value of temp (original x) to y
end;

var
  a, b: integer;

begin
  a := 10;
  b := 20;

  WriteLn('Before swapping: a = ', a, ', b = ', b);

  SwapNumbers(a, b);  // Call the procedure to swap the values

  WriteLn('After swapping: a = ', a, ', b = ', b);
  ReadLn;
end.
```

### Function

A `function` is similar to a procedure but it returns a value. Use this when you need to perform a task and get a result back.

**Syntax**

```pascal linenums="1"
function FunctionName(AParam:Type;...): Type;
const
  { const section }
var
  { var section }
begin
  { your code goes here }
  Result := {value to return} 
end;
```

**Example - Function without parameters**

```pascal linenums="1" hl_lines="4-7"
program ExampleFunctionWithoutParams;
{$mode objfpc}{$H+}{$J-}

function GetGreeting: string;
begin
  Result := 'Hello, World!';
end;

begin
  WriteLn(GetGreeting);  // Calling the function and printing the result
  ReadLn;
end.
```

**Example - Function with parameters**

```pascal linenums="1" hl_lines="4-7"
program ExampleFunctionWithParams;
{$mode objfpc}{$H+}{$J-}

function Add(a, b: integer): integer;
begin
  Result := a + b; // Returning the product
end;

begin
  WriteLn('3 + 5 = ', Add(3, 5));
  ReadLn;
end.
```

**Example - Function with `var` section**

```pascal linenums="1" hl_lines="4-12"
program ExampleFunctionWithVarSection;
{$mode objfpc}{$H+}{$J-}

function Factorial(n: integer): integer;
var
  i, tempCalc: integer;
begin
  tempCalc := 1;
  for i := 1 to n do
    tempCalc := tempCalc * i;
  Result := tempCalc;
end;

begin
  WriteLn('Factorial of 5 is: ', Factorial(5));
  ReadLn;
end.
```

## 8. Loops

### The `for..to` Statement

**Syntax**

```pascal linenums="1"
for counter := startValue to endValue do
begin
  // statements
end;
```

**Example of `for..to..do` loop**

```pascal linenums="1"
program ForToLoop;
{$mode objfpc}{$H+}{$J-}

var
  i: integer;

begin
  for i := 1 to 10 do
  begin
    WriteLn('i = ', i);
  end;
  ReadLn;
end.
```

**Example of `for..downto..do` Loop**

```pascal linenums="1"
program ForDowntoLoop;
{$mode objfpc}{$H+}{$J-}

var
  i: integer;

begin
  for i := 10 downto 1 do
  begin
    WriteLn('i = ', i);
  end;
  ReadLn;
end.
```

### The `for..in` Statement

**Syntax**

```pascal
for element in collection do
begin
  // Your code here
end;
```

**Example**

```pascal
program ForInLoop;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  numbers: array of integer;
  num: integer;

begin
  // Initialize the array
  numbers := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  // Use the for..in loop to iterate over the array
  for num in numbers do
  begin
    WriteLn('Number: ', num);
  end;

  // Pause console
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

## 9. Conditional Loops 

### The `while` Statement

The `while` loop is used to repeat a block of statements as long as a condition is true.

**Syntax**

```pascal linenums="1"
while condition do
begin
  // statements
end;
```

**Example**

```pascal linenums="1"
program WhileLoopExample;
{$mode objfpc}{$H+}{$J-}

var
  i: integer;

begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn('i = ', i);
    i := i + 1;
  end;
  ReadLn;
end.
```

### The `repeat` Statement

The `repeat..until` loop is similar to the `while` loop, but the condition is checked after the loop has executed, ensuring the statements are executed at least once.

**Syntax**

```pascal linenums="1"
repeat
  // statements
until condition;
```

**Example**

```pascal linenums="1"
program RepeatUntilLoopExample;
{$mode objfpc}{$H+}{$J-}

var
  i: integer;

begin
  i := 1;
  repeat
    WriteLn('i = ', i);
    i := i + 1;
  until i > 10;
  ReadLn;
end.
```

## 10. Choices and Decisions 

### The `case` Statement

**Syntax**

```pascal linenums="1"
case expression of
  value1: 
    begin
      // statements for value1
    end;
  value2: 
    begin
      // statements for value2
    end;
  value3, value4: 
    begin
      // statements for value3 and value4
    end;
  else
    begin
      // statements if none of the above values match
    end;
end;
```

**Example**

```pascal linenums="1"
program CaseStatementExample;
{$mode objfpc}{$H+}{$J-}

var
  grade: char;

begin
  Write('Enter a grade (A, B, C, D or F): ');
  ReadLn(grade);

  case grade of
    'A': 
      begin
        WriteLn('Excellent!');
      end;
    'B': 
      begin
        WriteLn('Good job!');
      end;
    'C': 
      begin
        WriteLn('Well done.');
      end;
    'D': 
      begin
        WriteLn('You passed.');
      end;
    'F': 
      begin
        WriteLn('You failed!');
      end;
    else
      begin
        WriteLn('Invalid grade');
      end;
  end;
  ReadLn;
end.
```

### The `if` Statement

**Syntax**

```pascal linenums="1"
if condition then
begin
  // statements to execute if condition is true
end;
```

Optionally, you can include an else part to execute a different block of code if the condition is false.

```pascal linenums="1"
if condition then
begin
  // statements to execute if condition is true
end
else
begin
  // statements to execute if condition is false
end;
```

For multiple conditions, you can use else if:

```pascal linenums="1"
if condition1 then
begin
  // statements to execute if condition1 is true
end
else if condition2 then
begin
  // statements to execute if condition2 is true
end
else
begin
  // statements to execute if none of the conditions are true
end;
```

**Example of `if..then` Statement**

```pascal linenums="1"
program IfThenExample;
{$mode objfpc}{$H+}{$J-}

var
  age: integer;

begin
  Write('Enter your age: ');
  ReadLn(age);

  if age >= 18 then
  begin
    WriteLn('You are an adult.');
  end;
  ReadLn;
end.
```

**Example of `if..then..else` Statement**

```pascal linenums="1"
program IfThenElseExample;
{$mode objfpc}{$H+}{$J-}

var
  age: integer;

begin
  Write('Enter your age: ');
  ReadLn(age);

  if age >= 18 then
  begin
    WriteLn('You are an adult.');
  end
  else
  begin
    WriteLn('You are a minor.');
  end;
  ReadLn;
end.
```

**Example of many conditions with `else if` Statement**

```pascal linenums="1"
program ElseIfExample;
{$mode objfpc}{$H+}{$J-}

var
  grade: char;

begin
  Write('Enter your grade (A, B, C, D, F): ');
  ReadLn(grade);

  if grade = 'A' then
  begin
    WriteLn('Excellent!');
  end
  else if grade = 'B' then
  begin
    WriteLn('Good job!');
  end
  else if grade = 'C' then
  begin
    WriteLn('Well done.');
  end
  else if grade = 'D' then
  begin
    WriteLn('You passed.');
  end
  else if grade = 'F' then
  begin
    WriteLn('Failed subject!');
  end
  else
  begin
    WriteLn('Invalid grade.');
  end;
  ReadLn;
end.
```

## 11. Math Operations

| Operator/Function     | Description                                                       |
|-----------------------|-------------------------------------------------------------------|
| `+`  (Addition)       | Adds numbers together.                                            |
| `-`  (Subtraction)    | Subtracts one number from another.                                |
| `*`  (Multiplication) | Multiplies numbers.                                               |
| `Power`               | Require unit `Math`. Raises one number to the power of another.   |
| `div` (Division)      | Divides numbers and returns the whole number part of the result.  |
| `/`   (Real Division) | Divides numbers and includes the decimal part of the result.      |
| `LogN(n, a)`          | Require unit `Math`. Calculates the logarithm base n of a number. |
| `mod` (Modulus)       | Returns the remainder when one number is divided by another.      |

**Example**

```pascal linenums="1"
program BasicMathOperations;

{$mode objfpc}{$H+}{$J-}

uses
  Math; // Include the Math unit for Power and Logarithm functions

var
  a, b, intResult: integer;
  realResult: real;

begin
  // Assign values to variables
  a := 10;
  b := 3;

  // Addition
  intResult := a + b;
  WriteLn('Addition: ', a, ' + ', b, ' = ', intResult);

  // Subtraction
  intResult := a - b;
  WriteLn('Subtraction: ', a, ' - ', b, ' = ', intResult);

  // Multiplication
  intResult := a * b;
  WriteLn('Multiplication: ', a, ' * ', b, ' = ', intResult);

  // Power
  realResult := Power(a, b); // 'Power' is used to calculate a^b
  WriteLn('Power: ', a, ' ^ ', b, ' = ', realResult: 0: 0);

  // Division
  intResult := a div b; // 'div' is used for integer division
  WriteLn('Division: ', a, ' div ', b, ' = ', intResult);

  // Real (Float) Division
  realResult := a / b; // '/' is used for real division
  WriteLn('Real (Float) Division: ', a, ' / ', b, ' = ', realResult: 0: 6);

  // Logarithm with Arbitrary Base (LogN)
  realResult := LogN(b, a); // 'LogN' is used to calculate logarithm base b
  WriteLn('Logarithm Base ', b, ': LogN(', b, ', ', a, ') = ', realResult: 0: 6);

  // Modulus
  intResult := a mod b; // 'mod' is used to find the remainder
  WriteLn('Modulus: ', a, ' mod ', b, ' = ', intResult);

  // Pause console
  WriteLn('Press enter key to exit');
  ReadLn;
end.
```

## 12. Round Floats 

### To Nearest Integers

You can use the following functions.

- [`Round`](https://www.freepascal.org/docs-html/rtl/system/round.html): Rounds a floating-point number to the nearest integer uses banker's rounding.
- [`Ceil`](https://www.freepascal.org/docs-html/rtl/math/ceil.html): Rounds a floating-point number up to the nearest integer.
- [`Floor`](https://www.freepascal.org/docs-html/rtl/math/floor.html): Rounds a floating-point number down to the nearest integer.

!!! Note

    For `Round`, in the case of .5 (equidistant from two numbers), the algorithm uses "banker's rounding": .5 values are always rounded towards the even number.

    Source: [https://www.freepascal.org/docs-html/rtl/system/round.html](https://www.freepascal.org/docs-html/rtl/system/round.html)

!!! Note
    Remember to add `Math` in the `uses` section `Ceil` and `Floor` functions.

**Examples**

```pascal linenums="1"
program RoundingExamples;

{$mode objfpc}{$H+}{$J-}

uses
  Math;  // Include the Math unit for Ceil and Floor

var
  num: real;
  rounded: integer;

begin
  num := 123.4567;

  // Using Round
  rounded := Round(num);  // Nearest integer, Banker's Rounding
  WriteLn('Rounded value (Round): ', rounded);

  // Using Ceil
  rounded := Ceil(num);   // Always rounds up
  WriteLn('Ceiling value (Ceil): ', rounded);

  // Using Floor
  rounded := Floor(num);  // Always rounds down
  WriteLn('Floor value (Floor): ', rounded);

  // Examples of Banker's Rounding
  num := 2.5;
  rounded := Round(num);  // Banker's Rounding
  WriteLn('Rounded value for 2.5 (Banker''s Rounding): ', rounded);

  num := 3.5;
  rounded := Round(num);  // Banker's Rounding
  WriteLn('Rounded value for 3.5 (Banker''s Rounding): ', rounded);

  // Pause console
  WriteLn('Press enter key to exit');
  ReadLn;
end.
```

### To `n` Decimal Places

Use the [RoundTo](https://www.freepascal.org/docs-html/rtl/math/roundto.html) function.

!!! Note

    `RoundTo` uses the standard `Round` function for this. Hence, in the case of .5 (equidistant from two numbers), the algorithm uses "banker's rounding": .5 values are always rounded towards the even number.
    
    Source: [https://www.freepascal.org/docs-html/rtl/math/roundto.html](https://www.freepascal.org/docs-html/rtl/math/roundto.html)

**Example**

```pascal linenums="1"
program NDecimalRoundingExample;

{$mode objfpc}{$H+}{$J-}

uses
  Math;

var
  num: real;
  rounded: real;
  n: integer;

begin
  num := 12345.678875;
  n := 4;  // Number of decimal places you want

  rounded := RoundTo(num, -n);

  WriteLn('Rounded Number: ', rounded: 0: 4);  // Format to 4 decimal places

  // Pause console
  WriteLn('Press enter key to exit');
  ReadLn;
end.
```

## 13. Bitwise Operations

| Operator | Description              | Example              | Binary Notation           | Result  |
|----------|--------------------------|----------------------|---------------------------|---------|
| `AND`    | Bitwise AND              | `5 and 3`            | `0101 and 0011`           | `0001`  |
| `OR`     | Bitwise OR               | `5 or 3`             | `0101 or 0011`            | `0111`  |
| `XOR`    | Bitwise XOR              | `5 xor 3`            | `0101 xor 0011`           | `0110`  |
| `NOT`    | Bitwise NOT (unary)      | `not 5`              | `not 0101`                | `...1010` (in 2's complement) |
| `SHL`    | Shift left               | `5 shl 1`            | `0101 shl 1`              | `1010`  |
| `<<`     | Shift left (equivalent)  | `5 << 1`             | `0101 << 1`               | `1010`  |
| `SHR`    | Shift right              | `5 shr 1`            | `0101 shr 1`              | `0010`  |
| `>>`     | Shift right (equivalent) | `5 >> 1`             | `0101 >> 1`               | `0010`  |

**Examples**

```pascal linenums="1"
program BitwiseOperatorsDemos;
{$mode objfpc}{$H+}{$J-}
uses
  SysUtils; // For BinStr

var
  a, b, result: integer;

begin
  a := 5;  // 0101 in binary
  b := 3;  // 0011 in binary

  // Bitwise AND
  result := a and b;  // 0101 and 0011 = 0001 (1 in decimal)
  writeln('AND: 5 and 3 = ', result, ' (', BinStr(result, 4), ')');

  // Bitwise OR
  result := a or b;  // 0101 or 0011 = 0111 (7 in decimal)
  writeln('OR: 5 or 3 = ', result, ' (', BinStr(result, 4), ')');

  // Bitwise XOR
  result := a xor b;  // 0101 xor 0011 = 0110 (6 in decimal)
  writeln('XOR: 5 xor 3 = ', result, ' (', BinStr(result, 4), ')');

  // Bitwise NOT
  result := not a;  // not 0101 = 1010 (in a 4-bit system, this is -6 in two's complement)
  writeln('NOT: not 5 = ', result, ' (', BinStr(result, 4), ')');

  // Shift left (SHL)
  result := a shl 1;  // 0101 shl 1 = 1010 (10 in decimal)
  writeln('SHL: 5 shl 1 = ', result, ' (', BinStr(result, 4), ')');

  // Shift left (<<) - same as SHL
  result := a << 1;  // 0101 << 1 = 1010 (10 in decimal)
  writeln('<< : 5 << 1 = ', result, ' (', BinStr(result, 4), ')');

  // Shift right (SHR)
  result := a shr 1;  // 0101 shr 1 = 0010 (2 in decimal)
  writeln('SHR: 5 shr 1 = ', result, ' (', BinStr(result, 4), ')');

  // Shift right (>>) - same as SHR
  result := a >> 1;  // 0101 >> 1 = 0010 (2 in decimal)
  writeln('>> : 5 >> 1 = ', result, ' (', BinStr(result, 4), ')');
  
  readln;
end.
```

Outputs

```bash linenums="1"
AND: 5 and 3 = 1 (0001)
OR: 5 or 3 = 7 (0111)
XOR: 5 xor 3 = 6 (0110)
NOT: not 5 = -6 (1010)
SHL: 5 shl 1 = 10 (1010)
<< : 5 << 1 = 10 (1010)
SHR: 5 shr 1 = 2 (0010)
>> : 5 >> 1 = 2 (0010)
```

## 14. Boolean Operations

| Operator | Operation          | Description                                                            |
|----------|--------------------|------------------------------------------------------------------------|
| `not`    | Logical negation    | Inverts the boolean value: `True` becomes `False`, and `False` becomes `True`. |
| `and`    | Logical conjunction | Returns `True` only if both operands are `True`.                       |
| `or`     | Logical disjunction | Returns `True` if at least one operand is `True`.                      |
| `xor`    | Exclusive OR        | Returns `True` if exactly one of the operands is `True`.               |

!!! Note

    By default, boolean expressions are evaluated with short-circuit evaluation.

    This means that from the moment the result of the complete expression is known, evaluation is stopped and the result is returned. 
    
    For instance, in the following expression:

    ```pascal
    B := True or MaybeTrue;
    ```
    
    The compiler will never look at the value of `MaybeTrue`, since it is obvious that the expression will always be `True`. 
    
    As a result of this strategy, if `MaybeTrue` is a function, ==it will not get called!== (This can have surprising effects when used in conjunction with properties).

    Adapted from: [https://www.freepascal.org/docs-html/current/ref/refsu46.html#x153-17700012.8.3](https://www.freepascal.org/docs-html/current/ref/refsu46.html#x153-17700012.8.3)

**Examples**

```pascal linenums="1"
program BooleanOperationsExample;
{$mode objfpc}{$H+}{$J-}
var
  a, b: boolean;
begin
  a := True;
  b := False;

  Writeln('not a: ', not a);           // Outputs: False
  Writeln('a and b: ', a and b);       // Outputs: False
  Writeln('a or b: ', a or b);         // Outputs: True
  Writeln('a xor b: ', a xor b);       // Outputs: True
  ReadLn;
end.
```

## 15. String Operations

Useful strings operators and functions.

| Operator/Function  | Description                                                                 |
|--------------------|-----------------------------------------------------------------------------|
| `+`                | Concatenates two strings.                                                   |
| `CompareStr`       | Case-sensitive string comparison. Returns 0 if strings are the same.        |
| `CompareText`      | Case-insensitive string comparison. Returns 0 if strings are the same.      |
| `Length`           | Returns the string length.                                                  |
| `Pos`              | Finds the position of a substring.                                          |
| `Copy`             | Extracts a substring from a string.                                         |
| `Delete`           | Removes part of a string.                                                   |
| `Insert`           | Inserts a substring at a specific position.                                 |
| `StringReplace`    | Replaces a substring with options for case sensitivity and all occurrences. |
| `str[n]`           | Accesses the character at the nth position.                                 |
| `UpperCase`        | Converts a string to uppercase.                                             |
| `LowerCase`        | Converts a string to lowercase.                                             |

**Examples**

```pascal linenums="1"
program StringOperationsExample;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  str1, str2, str3, result: string;
  position: integer;
  comparisonResult: integer;

begin
  str1 := 'Hello World';
  str2 := 'Welcome to Pascal';
  str3 := 'This is not easy!';

  // Concatenation
  result := str1 + ', ' + str2;
  Writeln('Concatenated string: ', result);
  // Outputs: Hello World, Welcome to Pascal

  // CompareStr (case-sensitive)
  comparisonResult := CompareStr(str1, 'hello world');
  Writeln('CompareStr result: ', comparisonResult);
  // Outputs: < 0 , since orc('H') < ord('h')

  // CompareText (case-insensitive)
  comparisonResult := CompareText(str1, 'hello world');
  Writeln('CompareText result: ', comparisonResult);
  // Outputs: 0,  since 'Hello World' = 'hello world' ignoring case

  // Length
  Writeln('Length of str1: ', Length(str1));
  // Outputs: 11

  // Pos
  position := Pos('World', str1);
  Writeln('Position of ''World'' in str1: ', position);
  // Outputs: 7

  // Copy
  result := Copy(str1, Pos('World', str1), Length('World'));
  Writeln('Copy ''World'' from str1: ', result);
  // Outputs: World

  // Delete
  Delete(str3, Pos('not ', str3), Length('not '));
  Writeln('After deleting ''NOT '' from str3: ', str3);
  // Outputs: This is easy

  // Insert
  Insert('really ', str3, pos('easy', str3));
  Writeln('After inserting ''really '' into str3: ', str3);
  // Outputs: This is really easy

  // StringReplace
  result := StringReplace(str1, 'Hello', 'Hello Free Pascal', [rfReplaceAll, rfIgnoreCase]);
  Writeln('After StringReplace to str1: ', result);
  // Outputs: Hello Free Pascal

  // Access character at position n
  Writeln('Character at pos 7 in str2: ', str2[7]);
  // Outputs: e

  // UpperCase
  result := UpperCase(str2);
  Writeln('UpperCase of str2: ', result);
  // Outputs: WELCOME TO FREE PASCAL

  // LowerCase
  result := LowerCase(str2);
  Writeln('LowerCase of str2: ', result);
  // Outputs: welcome to free pascal

  //Pause console
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

**Outputs**

```bash
Concatenated string: Hello World, Welcome to Pascal
CompareStr result: -32
CompareText result: 0
Length of str1: 11
Position of 'World' in str1: 7
Copy 'World' from str1: World
After deleting 'NOT ' from str3: This is easy!
After inserting 'really ' into str3: This is really easy!
After StringReplace to str1: Hello Free Pascal World
Character at pos 7 in str2: e
UpperCase of str2: WELCOME TO PASCAL
LowerCase of str2: welcome to pascal
Press enter key to quit
```

## 16. Format Strings

### Format Numbers with Commas

1. **Include the `SysUtils` unit**, as the `Format` function is part of this unit.
2. Use the `Format` function with appropriate format specifiers.

See [Format](https://www.freepascal.org/docs-html/rtl/sysutils/format.html) for more info.

**Example**

```pascal linenums="1" hl_lines="5 6 16"
program FormatNumberCommas;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  number: int64;
  formattedNumber: string;

begin
  // Formatting a number with commas
  number := 12345678;
  formattedNumber := Format('%.0n', [number * 1.0]);
  WriteLn('Formatted Number: ', formattedNumber);  // Output: 12,345,678

  // Pause console
  WriteLn('Press enter key to exit');
  ReadLn;
end.
```

- `'%.0n'` format specifier means *"format as a number with no decimal places, using the locale's thousands separator"*.

### Format Numbers as Currency

1. **Include the `SysUtils` unit**, as the `CurrToStrF` function is part of this unit.
2. Use the `CurrToStrF` function with appropriate format specifiers and decimal place.

See [`CurrToStrF`](https://www.freepascal.org/docs-html/3.2.2/rtl/sysutils/currtostrf.html) for more info.

**Example**

```pascal linenums="1" hl_lines="5 6 15"
program FormatCurrency;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  amount: currency;
  formattedAmount: string;

begin
  amount := 12345678.90;
  formattedAmount := CurrToStrF(amount, ffCurrency, 2);
  WriteLn(formattedAmount);  // Output: $12,345,678.90

  // Pause console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

- [`CurrToStrF`](https://www.freepascal.org/docs-html/3.2.2/rtl/sysutils/currtostrf.html) function: This formats a number as currency. The parameters are:
    - The number to format.
    - `ffCurrency`: A format specifier indicating that we want currency formatting.
    - `2`: The number of decimal places.

## 17. Processing Text Files

### Read a Text File

Here's an example to read a file line by line using `TFileStream` and `TStreamReader`:

!!! Note
    For this example to work, create a text file named `your-file.csv` (or any name you prefer, then update the code) in the same directory as your compiled program. Add some lines of text to it.

**Example**

```pascal linenums="1"
program SimpleReadTextFile;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  Classes,
  streamex;

var
  fileStream: TFileStream;
  streamReader: TStreamReader;
  line: string;

begin
  try
    // Open the file
    fileStream := TFileStream.Create('your-file.csv', fmOpenRead);
    try
      streamReader := TStreamReader.Create(fileStream, 65536, False);
      try
        // Read the file line by line
        while not streamReader.Eof do
        begin
          line := streamReader.ReadLine;
          // Process the line (for now, we just print it)
          Writeln(line);
        end;
      finally
        // Clean up
        streamReader.Free;
      end;
    finally
      // Clean up
      fileStream.Free;
    end;
  except
    on E: Exception do
      Writeln('An error occurred: ', E.Message);
  end;
  ReadLn; // Pause to see output or error
end.
```

- The `streamex` unit includes the `TStreamReader` class for handling text stream operations.
- The `try...finally` blocks guarantee that both `streamReader` and `fileStream` are properly released, even if an error occurs during the file reading process.
- Substitute `your-file.csv` with the name of your file.

### Write a Text File

**Example**

```pascal linenums="1"
program SimpleWriteTextFile;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils;

var
  text: string;
  filename: string;
  fileStream: TFileStream;
  size: longint;

begin
  try
    // String to be written
    text := 'Hello Text!' + LineEnding + 'I''ll be written in a file!';

    // Text file to write the text to
    filename := 'hello-text.txt';

    // Create a TFileStream
    fileStream := TFileStream.Create(filename, fmCreate);
    try
      // Set writing position at the beginning of file
      fileStream.Position := 0;
      // Write text into the file and return written bytes
      size := fileStream.Write(Text[1], Length(Text));
      // Optional - Show confirmation
      Writeln(Format('Created %s. %d bytes written.', [filename, size]));
    finally
      // Free TFileStream object
      fileStream.Free;
    end;
  except
    on E: Exception do
      Writeln('An error occurred: ', E.Message);
  end;
  ReadLn; // Pause to see output or error
end.
```

- The `try...finally` block ensures that the fileStream is properly closed and released even if an exception occurs during the file writing process.
- Update the content of the Text variable with the string you want to write to the file.
- Change `hello-text.txt` to the name of the file you wish to create or modify.
- The `try...except` block captures any exceptions that might occur during file operations and displays an appropriate error message.

## 18. Enum Types

In Free Pascal, enumerated ordinal types are user-defined types that consist of a set of named values. These values are called enumeration constants, and each constant has an associated integer value, starting from 0 by default. 

Enumerated types provide a way to define a variable that can only take one of a specified set of values, making the code more readable and type-safe.

**Syntax**

```pascal linenums="1"
type
  TEnumName = (Value1, Value2, Value3, ...);
```

**Example**

```pascal linenums="1"
program EnumExample;
{$mode objfpc}{$H+}{$J-}

type
  TDay = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

var
  today: TDay;

begin
  // Assign today var to TDay.Wednesday 
  today := Wednesday;
  
  WriteLn('Integer(today)      gives ', Integer(today));     // Prints 3
  WriteLn('Integer(Wednesday)) gives ', Integer(Wednesday)); // Prints 3
  WriteLn('TDay(0)             gives ', TDay(0));            // Prints Sunday

  if today = Wednesday then
  begin
    WriteLn('Today is Wednesday');
  end;
  ReadLn;
end.
```

**Example of an Enum in case statement**

```pascal linenums="1"
program EnumInCaseExample;
{$mode objfpc}{$H+}{$J-}

type
  TColor = (Red, Green, Blue);

var
  color: TColor;

begin
  color := Green;
  case color of
    Red: WriteLn('Red');
    Green: WriteLn('Green');
    Blue: WriteLn('Blue');
  end;
  ReadLn;
end.
```

## 19. Subrange Types

A subrange is a subset of values within a specific range. In Free Pascal, subranges allow you to limit the values a variable can hold, which can help catch errors and make your code more robust and readable.

**Syntax**

```pascal linenums="1"
type
  SubrangeType = LowValue..HighValue;
```

**Example**

```pascal linenums="1"
program SubrangeDaysofWeek;

{$mode objfpc}{$H+}{$J-}

type
  // Define a subrange type for days of the week (1 = Sunday, 7 = Saturday)
  TDayOfWeek = 1..7;

var
  // Declare a variable of type TDayOfWeek
  day: TDayOfWeek;

begin
  // Assign a valid value within the subrange to the variable
  day := 3;  // 3 represents Tuesday
  WriteLn('Day of the week is: ', day);

  // Uncommenting the following line would cause a compile-time error
  // because 8 is not within the defined subrange
  // day := 8;  // This will cause a compile-time error

  // Pause console
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

!!! Note

    **Why Not Just Use Integer?**
    
    Using a subrange like `TDayOfWeek` instead of a plain integer provides type safety. It ensures that the variable `day` can only be assigned values within the defined range (`1` to `7`). 
    This helps prevent errors and makes your code more robust and readable. For example, if you accidentally try to assign a value outside the range, the compiler will catch the error.

## 20. Arrays

Arrays are useful when you need to handle multiple values of the same type. For example, if you have grades for students, you can use an array to store all these grades and easily access each one by its position.

### Defining Arrays

**1. Directly in the var section**

```pascal linenums="1"
var
  numbers: array[1..5] of integer;
```

**2. Using the type section**

```pascal linenums="1"
type
  TNumberArray = array[1..5] of Integer;

var
  numbers: TNumberArray;
```

### Working with Arrays

**Example**

```pascal linenums="1"
program WorkingWithArraysExample;
{$mode objfpc}{$H+}{$J-}
var
  numbers: array[1..5] of Integer;

begin
  numbers[1] := 10;  // Set the first element to 10
  numbers[2] := 20;  // Set the second element to 20
  WriteLn('numbers[1] = ', numbers[1]); // This will print 10
  WriteLn('numbers[2] = ', numbers[2]); // This will print 20
  ReadLn;
end.
```

### Static Arrays

Static arrays have a fixed size defined at compile time.

**Syntax**

```pascal linenums="1"
var
  arrayName: array[startIndex..endIndex] of elementType;
```

**Example**

```pascal linenums="1"
program StaticArrayExample;

{$mode objfpc}{$H+}{$J-}

var
  numbers: array[1..5] of integer;
  i: integer;

begin
  // Initialising the array
  numbers[1] := 10;
  numbers[2] := 20;
  numbers[3] := 30;
  numbers[4] := 40;
  numbers[5] := 50;

  // Accessing and printing array elements
  for i := 1 to 5 do
    WriteLn('numbers[', i, '] = ', numbers[i]);
  ReadLn;
end.
```

### Dynamic Arrays

Dynamic arrays can be resized at runtime using the `SetLength` procedure.

**Syntax**

```pascal linenums="1"
var
  arrayName: array of elementType;
```

**Example**

```pascal linenums="1"
program DynamicArrayExample;

{$mode objfpc}{$H+}{$J-}

var
  numbers: array of integer;
  i: integer;

begin
  // Setting the length of the array
  SetLength(numbers, 5);

  // Initialising the array
  for i := 0 to High(numbers) do
    numbers[i] := (i + 1) * 10;

  // Accessing and printing array elements
  for i := 0 to High(numbers) do
    WriteLn('numbers[', i, '] = ', numbers[i]);

  // Resizing the array
  SetLength(numbers, 10);
  for i := 5 to 9 do
    numbers[i] := (i + 1) * 10;

  // Accessing and printing array elements after resizing
  for i := 0 to High(numbers) do
    WriteLn('numbers[', i, '] = ', numbers[i]);
  ReadLn;
end.
```

### Concat Dynamic Arrays

This operator is available in Delphi mode, but must be enabled explicily using the modeswitch arrayoperators in objfpc mode:

```pascal linenums="1"
{$mode objfpc}  
{$modeswitch arrayoperators}
```

**Syntax**

```pascal linenums="1"
resultArray := array1 + array2;
```

**Example**

```pascal linenums="1" hl_lines="4 33"
program DynArrayConcat;

{$mode objfpc}{$H+}{$J-}
{$modeswitch arrayoperators}

uses
  SysUtils;

type
  TIntArray = array of integer;

procedure PrintArray(arr: TIntArray);
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    Write(arr[i], ' ');
  WriteLn;
end;

var
  arr1, arr2, resultArr: TIntArray;

begin
  // Initialize the first array
  arr1 := [1, 2, 3, 4, 5];

  // Initialize the second array
  arr2 := [6, 7, 8, 9, 10];

  // Concatenate the arrays using the + operator
  resultArr := arr1 + arr2;

  // Print the arrays
  WriteLn('Array 1:');
  PrintArray(arr1);
  WriteLn('Array 2:');
  PrintArray(arr2);
  WriteLn('Concatenated Array:');
  PrintArray(resultArr);

  // Pause console
  ReadLn;
end.
```

See more info on the [Dynamic Array Operators](https://www.freepascal.org/docs-html/ref/refsu48.html) document.

### Open Arrays

Open arrays are typically used in procedures or functions to accept arrays of varying sizes.

**Syntax**

```pascal linenums="1"
procedure ProcedureName(arrayName: array of elementType);
```

**Example**

```pascal linenums="1"
program OpenArrayExample;

{$mode objfpc}{$H+}{$J-}

procedure PrintArray(arr: array of integer);
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    WriteLn('arr[', i, '] = ', arr[i]);
end;

var
  numbers: array[1..5] of integer;

begin
  // Initialising the array
  numbers[1] := 10;
  numbers[2] := 20;
  numbers[3] := 30;
  numbers[4] := 40;
  numbers[5] := 50;

  // Passing the array to the procedure
  PrintArray(numbers);
  ReadLn;
end.
```

## 21. Records Types

Just for the `Record`, a `record` is a data structure that allows you to group different types of data together. This feature in Free Pascal allow you to create complex data structures and manage related data efficiently.

**Syntax**

```pascal linenums="1"
type
  TRecordName = record
    field1: dataType1;
    field2: dataType2;
    field3: dataType3;
    // Add more fields as needed
  end;
```

**Example**

```pascal linenums="1"
program RecordExample;
{$mode objfpc}{$H+}{$J-}

type
  TPerson = record
    name: string;
    age: integer;
    height: real;
  end;

var
  person1, person2: TPerson;

begin
  // Assign values to the fields of Person1
  person1.Name := 'Javert';
  person1.Age := 30;
  person1.Height := 5.9;

  // Print the values of Person1
  WriteLn('Person1 Name: ', person1.Name);
  WriteLn('Person1 Age: ', person1.Age);
  WriteLn('Person1 Height: ', person1.Height:0:2);

  // Assign values to the fields of Person2
  person2.Name := 'Jean Valjean';
  person2.Age := 25;
  person2.Height := 5.7;

  // Print the values of Person2
  WriteLn('Person2 Name: ', person2.Name);
  WriteLn('Person2 Age: ', person2.Age);
  WriteLn('Person2 Height: ', person2.Height:0:2);
  ReadLn;
end.
```

## 22. Advanced Records

In Free Pascal, an advanced record is a type of record that can do more than just store data. It can also have methods (which are like functions or procedures) and properties (ways to get or set values) attached to it.

You must include the following switch to use Advanced Records.

```pascal linenums="1"
{$modeswitch advancedrecords}
```

**Syntax**

```pascal linenums="1"
type
  TMyRecord = record
  private
    // Private fields and methods
    FField: integer;
    procedure SetField(Value: integer);
    function GetField: integer;
  public
    // Public methods and properties
    procedure ShowInfo;
    property Field: integer read GetField write SetField;
  end;
```

**Example: A Simple TRectangle Record**

Let’s create an advanced record to represent a rectangle. This record will store the width and height of the rectangle and include methods to calculate the area and display the rectangle's details.

We’ll create a record called `TRectangle` that has fields for width and height. It will also include a method to calculate the area and another to display the details.

```pascal linenums="1"
program AdvancedRecordDemo;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

type
  TRectangle = record
  private
    FWidth, FHeight: Double;
    procedure SetWidth(Value: Double);
    procedure SetHeight(Value: Double);
    function GetWidth: Double;
    function GetHeight: Double;
  public
    constructor Create(AWidth, AHeight: Double);
    function Area: Double;
    procedure ShowDetails;
    property Width: Double read GetWidth write SetWidth;
    property Height: Double read GetHeight write SetHeight;
  end;

constructor TRectangle.Create(AWidth, AHeight: Double);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TRectangle.SetWidth(Value: Double);
begin
  FWidth := Value;
end;

procedure TRectangle.SetHeight(Value: Double);
begin
  FHeight := Value;
end;

function TRectangle.GetWidth: Double;
begin
  Result := FWidth;
end;

function TRectangle.GetHeight: Double;
begin
  Result := FHeight;
end;

function TRectangle.Area: Double;
begin
  Result := FWidth * FHeight;
end;

procedure TRectangle.ShowDetails;
begin
  WriteLn('Rectangle Width: ', FWidth:0:2);
  WriteLn('Rectangle Height: ', FHeight:0:2);
  WriteLn('Rectangle Area: ', Area:0:2);
end;

var
  Rect: TRectangle;

begin
  // Create a rectangle with width 10 and height 5
  Rect := TRectangle.Create(10, 5);
  
  // Show the details of the rectangle
  Rect.ShowDetails;
  ReadLn;
end.
```

## 23. Classes

Here is a simple example of creating a class. For more info, visit the official documentation; [Classes](https://www.freepascal.org/docs-html/ref/refch6.html#x69-930006). 

**Syntax**

```pascal linenums="1"
type
  TMyClass = class
  private
    // Private fields and methods
  protected
    // Protected fields and methods
  public
    // Public fields and methods
    constructor Create; // Constructor
    destructor Destroy; override; // Destructor
  end;
```

**Example**

```pascal linenums="1"
program ClassExample;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}


type
  // Define the class
  TPerson = class
  private
    FName: string;
    FAge: integer;
  public
    constructor Create(const AName: string; AAge: integer);
    procedure DisplayInfo;
    property Name: string read FName write FName;
    property Age: integer read FAge write FAge;
  end;

// Implementation of the constructor
constructor TPerson.Create(const AName: string; AAge: integer);
begin
  FName := AName;
  FAge := AAge;
end;

// Implementation of the method to display information
procedure TPerson.DisplayInfo;
begin
  WriteLn('Name: ', FName);
  WriteLn('Age: ', FAge);
end;

var
  Person: TPerson;

begin
  // Create an instance of TPerson
  Person := TPerson.Create('John Doe', 28);

  // Access properties
  Person.Name := 'Waldo Catto';
  Person.Age := 18;

  // Display information
  Person.DisplayInfo;

  // Free the memory used by the instance
  Person.Free;

  // Pause console
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

## 24. Generics

**Why use this?** Generics allow you to write reusable code that works with different data types without duplication. For example, instead of writing separate functions for adding two integers, two real numbers, and two strings, you can write one generic function that works for all types. This reduces code duplication and makes maintenance easier.

Generics allow you to write code that can work with different data types without having to rewrite the same code for each type.

### Generic Routines

**Syntax**

```pascal linenums="1"
generic function GenericFunction<T>(AParam: T; ... ): T;
begin
  // Function body
  Result := value;
end;
```

**Example**

```pascal linenums="1"
program GenericFunctionExample;

{$mode objfpc}{$H+}{$J-}

generic function DoubleValue<T>(AValue: T): T;
begin
  Result := AValue + AValue;
end;

var
  resultInt: integer;
  resultReal: real;

begin
  resultInt := specialize DoubleValue<integer>(8);
  Writeln('resultInt:  ', resultInt);         // Output: resultInt: 64

  resultReal := specialize DoubleValue<real>(-1.2);
  Writeln('resultReal: ', resultReal: 0: 2); // Output: resultReal: -2.40

  // Pause console
  WriteLn('Press Enter to quit');
  ReadLn;
end.
```

### Generic Records

**Syntax**

```pascal linenums="1"
type
  TRecordName<T> = record
    // Record body
  end;

  TMyType = specialize TRecordName<DataType>;
```

**Example**

```pascal linenums="1"
program GenericRecordExample;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

type
  // Define a generic record TPair
  generic TPair<T1, T2> = record
    First: T1;
    Second: T2;
    constructor Create(AFirst: T1; ASecond: T2);
  end;

  constructor TPair.Create(AFirst: T1; ASecond: T2);
  begin
    First := AFirst;
    Second := ASecond;
  end;

type
  // Create types based on TPair
  TIntegerStringPair = specialize TPair<integer, string>;
  TRealBoolPair = specialize TPair<real, boolean>;

var
  intStrPair: TIntegerStringPair;
  realBoolPair: TRealBoolPair;

begin
  intStrPair := TIntegerStringPair.Create(10, 'Ten');
  realBoolPair := TRealBoolPair.Create(3.14, True);

  Writeln('intStrPair  : (', intStrPair.First, ', ', intStrPair.Second, ')');
  Writeln('realBoolPair: (', realBoolPair.First: 0: 2, ', ', realBoolPair.Second, ')');

  // Pause console
  WriteLn('Press Enter to quit');
  ReadLn;
end.
```

Alternatively, use `specialize` directly in the var section.

```pascal
var
  intStrPair:specialize TPair<integer, string>;
  realBoolPair:specialize TPair<real, boolean>;
```

And use `specialize` again when you initialise the variables

```pascal
begin
  // ...
  intStrPair := specialize TPair<integer, string>.Create(10, 'Ten');
  realBoolPair := specialize TPair<double, boolean>.Create(3.14, True);
  // ...
end.
```

### Generic Classes

**Syntax**

```pascal linenums="1"
type
  TClassName<T> = class
    // Class body
  end;

  TMyType = specialize TClassName<DataType>;
```

**Example**

```pascal linenums="1"
program GenericClassExample;

{$mode objfpc}{$H+}{$J-}

type
  // Define a generic class TSimpleCalculator
  generic TSimpleCalculator<T> = class
  public
    function Add(A, B: T): T;
    function Subtract(A, B: T): T;
  end;

  function TSimpleCalculator.Add(A, B: T): T;
  begin
    Result := A + B;
  end;

  function TSimpleCalculator.Subtract(A, B: T): T;
  begin
    Result := A - B;
  end;

type
  // Define specific calculator types based on the generic class
  TIntCalculator = specialize TSimpleCalculator<integer>;
  TRealCalculator = specialize TSimpleCalculator<double>;

var
  intCalc: TIntCalculator;
  floatCalc: TRealCalculator;

begin
  // Create a calculator for integers
  intCalc := TIntCalculator.Create;
  Writeln('Integer Calculator:');
  Writeln('5 + 3 = ', intCalc.Add(5, 3));
  Writeln('5 - 3 = ', intCalc.Subtract(5, 3));

  // Create a calculator for floating-point numbers
  floatCalc := TRealCalculator.Create;
  Writeln('Float Calculator:');
  Writeln('5.5 + 3.2 = ', floatCalc.Add(5.5, 3.2): 0: 2);
  Writeln('5.5 - 3.2 = ', floatCalc.Subtract(5.5, 3.2): 0: 2);

  // Free the calculators
  intCalc.Free;
  floatCalc.Free;

  // Pause console
  WriteLn('Press Enter to quit');
  ReadLn;
end.
```

## 25. Function References

!!! Note
    Function References is available for FPC versions >= 3.3.1. 

Official doc: [https://forum.lazarus.freepascal.org/index.php/topic,59468.msg443370.html#msg443370](https://forum.lazarus.freepascal.org/index.php/topic,59468.msg443370.html#msg443370).

Enable function references using the modeswitch FUNCTIONREFERENCES.

```pascal
{$modeswitch functionreferences}
```

**Syntax**

```pascal linenums="1"
type 
  funcRef = reference to function|procedure [(argumentlist)][: resulttype;] [directives;]
```

**Example - A simple Function Reference**

```pascal linenums="1"
program FuncRefEx1;

{$mode objfpc}{$H+}{$J-}
{$modeswitch functionreferences}

type
  // Define a type for a function reference that take two doubles and return a double
  TMathOpFuncRef = reference to function(a, b: double): double;

  // Define a function that adds two doubles
  function Add(a, b: double): double;
  begin
    Result := a + b;
  end;

  // Define a function that multiplies two doubles
  function Multiply(a, b: double): double;
  begin
    Result := a * b;
  end;

var
  // A variable of type TMathOpFuncRef to hold function references
  mathOp: TMathOpFuncRef;
  num1, num2: double;

begin
  num1 := 5;
  num2 := 3;

  // Assign the Add function to the mathOp function reference
  mathOp := @Add;
  WriteLn(num1:0:2, ' + ', num2:0:2, ' = ', mathOp(num1, num2):0:2);

  // Assign the Multiply function to the mathOp function reference
  mathOp := @Multiply;
  WriteLn(num1:0:2, ' * ', num2:0:2, ' = ', mathOp(num1, num2):0:2);

  // Pause console
  WriteLn('Press Enter to quit');
  ReadLn;
end.
```

## 26. Anonymous Functions

!!! Note
    Anonymous Functions is available for FPC versions >= 3.3.1.

Official doc: [https://forum.lazarus.freepascal.org/index.php/topic,59468.msg443370.html#msg443370](https://forum.lazarus.freepascal.org/index.php/topic,59468.msg443370.html#msg443370).

Enable anonymous functions using the modeswitch ANONYMOUSFUNCTIONS.

```pascal
{$modeswitch anonymousfunctions}
```

**Syntax**

```pascal linenums="1"
function|procedure [(argumentList)][[resultName]: resultType;] [directives;]
[[var|type|const section]|[nested routine]]*
begin
[statements]
end
```

**Example**

```pascal linenums="1"
program AnonymousFuncSimple;

{$mode objfpc}{$H+}{$J-}
{$modeswitch anonymousfunctions}  // Enable anonymous functions

uses
  SysUtils, Classes;

type
  TFunc = function: integer;

var
  proc: TProcedure;     // Declared in SysUtils
  func: TFunc;
  notify: TNotifyEvent; // Declared in Classes

begin

  // Anonymous procedure with a single argument
  procedure(const aArg: string)
  begin
    Writeln(aArg);
  end('Hello World');

  // Assigning an anonymous procedure to the 'proc' variable
  proc := procedure
          begin
            Writeln('Foobar');
          end;
  proc;

  // Assigning an anonymous procedure to the 'notify' event
  notify := procedure(aSender: TObject)
            begin
              Writeln(HexStr(Pointer(aSender)));
            end;
  notify(Nil);

  // Assigning an anonymous function to the 'func' variable
  func := function MyRes : integer
          begin
            Result := 42;
          end;
  Writeln(func);

  // Pause the console to view the output
  WriteLn('Press Enter to quit');
  ReadLn;
end.
```

## 27. Interfaces

**Why use this?** Interfaces define a "contract" that classes must follow. They're useful when you have multiple classes that should perform similar tasks but in different ways. For example, you might have a `IAnimal` interface that guarantees any animal can eat, sleep, and make a sound. Different animal classes (Dog, Cat, Bird) can implement this interface in their own way. Interfaces promote code reusability and flexibility.

!!! Note
    By default, Free Pascal uses the Windows COM `IUnknown` interface type. This is important for compatibility with other systems, especially on Windows. For simpler, non-COM interfaces, you might see them declared without descending from `IUnknown` or `TInterfacedObject`, but this changes how memory management (like reference counting) works.

Think of an interface as a contract or a blueprint. It defines *what* a class should be able to do (methods it must have) but not *how* it does them. This allows different classes to promise they can perform certain actions, even if their internal workings are different.

- Interfaces are typically used in `delphi` or `objfpc` modes.
- All methods and properties in an interface are `public`.
- A class implementing an interface must provide code for all methods defined in that interface.
- **GUIDs (Globally Unique Identifiers)**: You'll often see interfaces defined with a long, unique string like `['{12345678-1234-1234-1234-1234567890AB}']`. This GUID ensures the interface is uniquely identifiable worldwide, which is crucial for systems like COM (Component Object Model) where different programs need to interact reliably. For simpler applications not using COM, the GUID might be omitted, but it's good practice for broadly usable interfaces.

!!! Info
    Refer to the official doc [Interfaces](https://www.freepascal.org/docs-html/ref/refch7.html#x96-1200007) for more info.

**Syntax**

1.  Define an interface using the `interface` keyword.
2.  Define a class that implements the interface. It usually inherits from `TInterfacedObject` (which helps with memory management for COM-style interfaces) and lists the interface(s) it implements.

**Example**

This example defines a simple interface `IMyInterface` with one method `DoSomething`, and then implements this interface in a class `TMyClass`.

**1. Define the Interface**

```pascal linenums="1"
type
  IMyInterface = interface
    ['{12345678-1234-1234-1234-1234567890AB}'] // Unique identifier (GUID)
    procedure DoSomething;
  end;
```

**2. Implement the Interface in a Class**

```pascal linenums="1"
type
  TMyClass = class(TInterfacedObject, IMyInterface)
  public
    procedure DoSomething;
  end;

procedure TMyClass.DoSomething;
begin
  WriteLn('Doing something...');
end;
```

**3. Use the Interface and Class**
The main program block would look like this:
```pascal linenums="1"
var
  MyObject: IMyInterface;
begin
  MyObject := TMyClass.Create; // Create an instance of the class
  MyObject.DoSomething;        // Call the interface method
  // MyObject is automatically managed if TMyClass descends from TInterfacedObject
  // No explicit MyObject.Free is needed here due to reference counting.
end.
```

**Complete Example**

```pascal linenums="1"
program InterfaceExample;

{$mode objfpc}{$H+}{$J-}

type
  // Step 1: Define the Interface
  IMyInterface = interface
    ['{12345678-1234-1234-1234-1234567890AB}'] // Unique identifier (GUID)
    procedure DoSomething;
  end;

  // Step 2: Implement the Interface in a Class
  TMyClass = class(TInterfacedObject, IMyInterface)
  public
    procedure DoSomething;
  end;

procedure TMyClass.DoSomething;
begin
  WriteLn('Doing something...');
end;

var
  MyObject: IMyInterface;
begin
  // Step 3: Use the Interface and Class
  MyObject := TMyClass.Create;
  MyObject.DoSomething;

  // Pause console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```
- `TInterfacedObject` is a base class from the `Classes` unit (implicitly used in many FPC programs) that helps manage memory for interfaces through reference counting. When an interface variable goes out of scope or is set to `nil`, if it's the last reference to the object, the object is automatically freed.

## 28. Pointers

**Why use this?** Pointers are advanced features that let you directly work with computer memory addresses. Most of the time, you don't need them—modern programming practices prefer safer alternatives like classes, dynamic arrays, and interfaces. However, pointers are essential when you need to work with external libraries, manage memory very carefully, or interface with lower-level system code. **Most beginners should avoid pointers until they're comfortable with other Free Pascal features.**

> ... Avoid pointer whenever alternatives exist. If you want to learn, though, there's no silver bullet apart from: There has to be as many `Dispose` as `New`, period.
>
> Source: [Leledumbo's reply on 'Dispose of Pointer', 2023-08-10](https://forum.lazarus.freepascal.org/index.php/topic,64238.msg487999.html#msg487999).
 
**Example**

```pascal linenums="1"
program PointerExample;

{$mode objfpc}{$H+}{$J-}

var
  ptr: ^integer;  // Declare a pointer to Integer
  value: integer;

begin
  New(ptr);           // Allocate memory for an Integer
  ptr^ := 42;         // Assign value 42 to the memory location
  value := ptr^;      // Access the value through the pointer

  Writeln('Value pointed to by ptr: ', value);

  Dispose(ptr);       // Free the allocated memory
  ReadLn;
end.
```

### Safe Usage Tips

1. **Always Initialize Pointers**: Before using a pointer, make sure it points to valid memory. Uninitialized pointers can cause undefined behavior.

2. **Check for nil**: It’s good practice to check if a pointer is nil (i.e., not pointing to any memory) before using it:

```pascal linenums="1"
if ptr <> nil then
  Writeln(ptr^);
```

3. **Avoid Memory Leaks**: Always pair `New` with `Dispose` to prevent memory leaks. If you forget to free the allocated memory, it will not be available for other parts of your program or system.

4. **Don’t Use Freed Pointers**: After calling `Dispose`, the pointer still holds the address of the freed memory. Set it to `nil` to avoid accidental use:

```pascal linenums="1"
Dispose(ptr);
ptr := nil;
```

5. **Be Cautious with Pointer Arithmetic**: Although not commonly needed in high-level Pascal programming, pointer arithmetic (e.g., incrementing pointers) should be done carefully to avoid accessing invalid memory areas.

More info? See [Pointers](https://www.freepascal.org/docs-html/ref/refse15.html#x42-620003.4) and [Memory Management](https://wiki.freepascal.org/Memory_Management).

---

## Glossary

### Basic Programming Terms

**Variable**: A named location in memory that stores a value. You declare variables to store data that your program will use.

**Data Type**: Specifies what kind of data a variable can hold (e.g., integer for whole numbers, string for text, boolean for true/false values).

**Constant**: A value that doesn't change during program execution. Unlike variables, you cannot modify a constant after it's declared.

**Function**: A block of code that performs a specific task and returns a value as a result. Use functions when you need a value back.

**Procedure**: Similar to a function, but it doesn't return a value. Use procedures when you just need to execute a series of statements without getting a result back.

### Object-Oriented Programming Terms

**Class**: A blueprint for creating objects. It defines what data (fields) an object can hold and what actions (methods) it can perform. Classes are used to model real-world entities in your code.

**Object/Instance**: A specific copy of a class created at runtime. If a class is a blueprint, an object is the actual building made from that blueprint.

**Constructor**: A special method that runs when you create an object. It sets up the initial state of the object. In Free Pascal, it's often called `Create`.

**Destructor**: A special method that runs when an object is being destroyed/freed from memory. It cleans up resources the object was using.

**Property**: A controlled way to access and modify data inside an object. Properties act like variables but allow you to run custom code when reading or writing the value.

**Method**: A function or procedure that belongs to a class. It's a way for objects to perform actions.

**Inheritance**: When one class (called the "child" or "derived" class) inherits features from another class (called the "parent" or "base" class). The child class automatically gets all the methods and fields of the parent.

**Encapsulation**: The practice of hiding internal details of an object and only exposing what's necessary. This is done using access modifiers like `private`, `protected`, and `public`.

**Access Modifiers**: Keywords that control who can access fields and methods:

- `private`: Only the class itself can access it.
- `protected`: The class and its descendants can access it.
- `public`: Anyone can access it.

### Advanced Programming Terms

**Interface**: A contract or blueprint that defines what methods a class must have, without specifying *how* those methods work. Different classes can implement the same interface in their own ways.

**Generic**: A way to write code that works with multiple data types. Instead of writing the same function for integers, strings, and real numbers, you write one generic function that adapts to the type you use it with.

**Pointer**: A variable that stores a memory address instead of a direct value. It "points to" the location where data is stored. Advanced feature—most beginners should avoid.

**Memory Address**: A location in your computer's memory where data is stored. Pointers hold these addresses.

**Reference Counting**: An automatic memory management system where the system tracks how many references point to an object. When the count reaches zero, the object is automatically freed.

**Anonymous Function**: A function without a name that's defined inline in your code. Useful for short, simple operations.

### Free Pascal Specific Terms

**Mode**: Refers to different compatibility modes in Free Pascal (e.g., `objfpc` mode for Object Pascal, `delphi` mode for Delphi compatibility). The mode determines which language features are available.

**Compiler Directive**: A special instruction (starting with `$`) that tells the compiler how to behave. For example, `{$mode objfpc}` sets the compiler to Object Pascal mode.

**Unit**: A module of code that can be reused in other programs. Units contain procedures, functions, types, and variables that you can use via the `uses` clause.

**GUID (Globally Unique Identifier)**: A long unique string of numbers that identifies something worldwide. In Free Pascal interfaces, GUIDs ensure interfaces are uniquely identifiable, especially important for COM compatibility.

**Dynamic Array**: An array whose size can change at runtime using `SetLength`. Unlike static arrays, you don't need to know the size when you declare it.

**Static Array**: An array whose size is fixed at compile time and cannot change.

**Enum (Enumerated Type)**: A custom data type where you define a set of named values. For example, you might create a `TColor` enum with values `Red`, `Green`, and `Blue`.

**Subrange Type**: A type that limits values to a specific range. For example, a `TDayOfWeek` might only allow values 1-7.

**Record**: A data structure that groups different types of data together. Unlike classes, records are simpler and don't have methods (unless advanced records are used).

**Modeswitch**: A compiler directive that enables specific language features. For example, `{$modeswitch advancedrecords}` enables advanced record functionality.

---

**Happy coding!** 🚀 If you encounter unfamiliar terms while learning, come back to this glossary. As you progress, these concepts will become second nature.
