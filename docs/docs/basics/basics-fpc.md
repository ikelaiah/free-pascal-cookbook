# Basics of Object Pascal using FPC

## Greetings!

This page is my go-to guide to the key basics of Object Pascal with the [Free Pascal Compiler](https://www.freepascal.org). It's not a complete guide, but it’s got all the essentials to help you dive in and discover just how powerful Free Pascal can be!

Hope you find it super helpful! 🚀

!!! Note

    This section assumes you have correctly set up the Free Pascal Compiler and the Lazarus IDE for your OS.

## 1. Hello, World! 

Here is a simple program that prints `Hello, World!` in Free Pascal.

```pascal linenums="1"
begin
  WriteLn('Hello, World!');
end.
```

!!! tip "Tip: Running from the Lazarus IDE"
    When running a console program in the Lazarus IDE, the console window may close too quickly to see the output. 
    
    Use `ReadLn` to keep it open until you press the ++enter++ key.

    ```pascal linenums="1" hl_lines="3"
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

Use `{` and `}` or `//` for making comments in Object Pascal.

You might use conmments as follows.

```pascal linenums="1"
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

```pascal linenums="1"
{ This is the main block }
begin
  { ... your code ... }
  
  // Example, print a message.
  WriteLn('Hello, World!');
end.
```

## 5. Variable and Basic Types

### Declaration

Declare variables within the `var` section of your program.

**Syntax**

```pascal linenums="1"
var
  variableName:dataType; 
```

**Example**

Here's a simple example of declaring basic variable types in Free Pascal.

```pascal linenums="1"
var
  myChar: char;       // A single character, like 'A' or 'b'
  myString: string;   // A sequence of characters, like "Hello, world!"
  myInt: integer;     // A whole number, like 42 or -7
  myBool: boolean;    // A true or false value
  myReal: real;       // A number with decimals, like 3.14 or -0.5
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
{$mode objfpc}{$H+}{$J-}
var
  num1, num2: integer;
begin
  { Using Read -- The next value read will be num 2. }
  Write('Enter two numbers: ');
  Read(num1);
  Read(num2);
  WriteLn('You entered: ', num1, ' and ', num2);
end.
```

Use `ReadLn` when you want to read input and move to the next line afterward.

```pascal linenums="1"
{$mode objfpc}{$H+}{$J-}
var
  num1, num2: integer;
begin
  { Using ReadLn -- The next value after a new line will be num 2. }
  Write('Enter two numbers on separate lines: ');
  ReadLn(num1);
  ReadLn(num2);
  WriteLn('You entered: ', num1, ' and ', num2);
end.
```

### Display Text

Similarly, `Write` and `WriteLn` are used to output text, but they behave differently.

Use `Write` to output text without moving to the next line. It keeps the cursor on the same line, so subsequent output will continue from where the previous output ended.

```pascal linenums="1"
{$mode objfpc}{$H+}{$J-}

begin
  { Using Write -- World! appears after Hello. }
  Write('Hello '); // No new line at the end of the string.
  Write('World!'); // No new line at the end of the string.

  // A spacer
  WriteLn;
end.
```

Use `WriteLn` to output text and then moves the cursor to the next line. It adds a newline character after the text, so any subsequent output starts on a new line.


```pascal linenums="1"
{$mode objfpc}{$H+}{$J-}

begin
  { Using WriteLn -- World! appears underneath Hello. }
  WriteLn('Hello '); // There is a new line at the end of the string.
  WriteLn('World!'); // There is a new line at the end of the string.
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

```pascal linenums="1" hl_lines="3-6"
program ExampleProcedureWithoutParams;

procedure Greet;
begin
  WriteLn('Hello, World!');
end;

begin
  Greet;  // Calling the procedure
end.
```

**Example - Procedure with Parameters**

```pascal linenums="1" hl_lines="3-6"
program ExampleProcedureWithParams;

procedure Greet(name: string);
begin
  WriteLn('Hello, ', name, '!');
end;

begin
  Greet('Alice'); // Calling the procedure with arguments
end.
```

**Example - Procedure with `var` section**

```pascal linenums="1" hl_lines="3-10"
program ExampleProcedureWithVarSection;

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

```pascal linenums="1" hl_lines="3-6"
program ExampleFunctionWithoutParams;

function GetGreeting: string;
begin
  Result := 'Hello, World!';
end;

begin
  WriteLn(GetGreeting);  // Calling the function and printing the result
end.
```

**Example - Function with parameters**

```pascal linenums="1" hl_lines="3-6"
program ExampleFunctionWithParams;

function Add(a, b: integer): integer;
begin
  Result := a + b; // Returning the product
end;

begin
  WriteLn('3 + 5 = ', Add(3, 5));
end.
```

**Example - Function with `var` section**

```pascal linenums="1" hl_lines="3-11"
program ExampleFunctionWithVarSection;

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
  {$mode objfpc}{$H+}{$J-}

var
  i: integer;

  // Main block
begin
  for i := 1 to 10 do
  begin
    WriteLn('i = ', i);
  end;
end.
```

**Example of `for..downto..do` Loop**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  i: integer;

  // Main block
begin
  for i := 10 downto 1 do
  begin
    WriteLn('i = ', i);
  end;
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

  // Main block
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
  {$mode objfpc}{$H+}{$J-}

var
  i: integer;

  // Main block
begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn('i = ', i);
    i := i + 1;
  end;
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
  {$mode objfpc}{$H+}{$J-}

var
  i: integer;

  // Main block
begin
  i := 1;
  repeat
    WriteLn('i = ', i);
    i := i + 1;
  until i > 10;
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
  {$mode objfpc}{$H+}{$J-}

var
  grade: char;

  // Main block
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
  {$mode objfpc}{$H+}{$J-}

var
  age: integer;

  // Main block
begin
  Write('Enter your age: ');
  ReadLn(age);

  if age >= 18 then
  begin
    WriteLn('You are an adult.');
  end;
end.
```

**Example of `if..then..else` Statement**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  age: integer;

  // Main block
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
end.
```

**Example of many conditions with `else if` Statement**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  grade: char;

  // Main block
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
end.
```


## 11.Math Operations

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

  // Main block
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

  // Main block
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

## 13. String Operations

### Length of a String

Use `Length(str)` to find the length of a string.

**Example**

```pascal linenums="1" hl_lines="12"
  {$mode objfpc}{$H+}{$J-}

var
  str:string;
  len:integer;

  // Main block
begin
  str := 'Hello, World!';
  
  // Get length of a string
  len:=Length(str);
  
  WriteLn('Length of ', str, ' is ', len); // Output: Length of Hello, World! is 12
end.
```

### Finding Character at n-th Index

**Syntax**

```pascal linenums="1"
stringVar[index];
```

**Example**

```pascal linenums="1" hl_lines="12"
  {$mode objfpc}{$H+}{$J-}

var
  str: string;
  ch: char;
  index: integer;

  // Main block
begin
  str := 'Hello, World!';
  index := 1; // Change this value to test different indices

  ch := str[index];
  WriteLn('Character at index ', index, ' is: ', ch);
end.
```

### Concat Strings

You can concat string using the `+` operator or `Concat(str1,str2)`.

**Example**

```pascal linenums="1" hl_lines="11 16"
  {$mode objfpc}{$H+}{$J-}

var
  str1, str2, result: string;

  // Main block
begin
  str1 := 'Hello, ';
  str2 := 'world!';
  
  // Using + operator
  result := str1 + str2;
  WriteLn(result); // Output: Hello, world!
  
  // Using Concat function
  result := Concat(str1, str2);
  WriteLn(result); // Output: Hello, world!
end.
```

### Replace Substring

1. Include `SysUtils` in the `uses` section.
2. Use `StringReplace(...)`.

**Example**

```pascal linenums="1" hl_lines="14"
  {$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  str, result: string;

  // Main block
begin
  str := 'Hello, Pascal!';
  
  // Replacing 'Pascal' with 'world'
  result := StringReplace(str, 'Pascal', 'world', [rfReplaceAll, rfIgnoreCase]);
  WriteLn(result); // Output: Hello, world!
end.
```


### Changing Case

1. Include `SysUtils` in the `uses` section.
2. Use `LowerCase(str)` or `UpperCase(str)`.

**Example**

```pascal linenums="1" hl_lines="4 14 18"
  {$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  str, lowerStr, upperStr: string;

  // Main block
begin
  str := 'Hello, Pascal!';
  
  // Convert to lowercase
  lowerStr := LowerCase(str);
  WriteLn(lowerStr); // Output: hello, pascal!
  
  // Convert to uppercase
  upperStr := UpperCase(str);
  WriteLn(upperStr); // Output: HELLO, PASCAL!
end.
```

### Searching for a Substring

You can use `Pos(substr, str)` to find a position of a substring in a string.

**Example**

```pascal linenums="1" hl_lines="13"
  {$mode objfpc}{$H+}{$J-}

var
  str, substr: string;
  index: integer;

  // Main block
begin
  str := 'Hello, Pascal!';
  substr := 'Pascal';
  
  // Find position of 'Pascal' in str
  index := Pos(substr, str);
  WriteLn('Position of "', substr, '" in "', str, '" is: ', index); // Output: 8
end.
```

## 14. Format Strings

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

  // Main block
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

  // Main block
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




## 15. Processing Text Files

### Read a Text File

Here's an example to read a file line by line using `TFileStream` and `TStreamReader`:

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

  // Main block
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
end.
```

- The `streamex` unit includes the `TStreamReader` class for handling text stream operations.
- The `try...finally` blocks guarantee that both `streamReader` and `fileStream` are properly released, even if an error occurs during the file reading process.
- Substitute `your-file.txt` with the name of your file.
- The `try...except` block handles any exceptions that arise during file operations and outputs an appropriate error message.


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

  // Main block
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
end.
```

- The `try...finally` block ensures that the fileStream is properly closed and released even if an exception occurs during the file writing process.
- Update the content of the Text variable with the string you want to write to the file.
- Change `hello-text.txt` to the name of the file you wish to create or modify.
- The `try...except` block captures any exceptions that might occur during file operations and displays an appropriate error message.


## 16. Enum Types

In Free Pascal, enumerated ordinal types are user-defined types that consist of a set of named values. These values are called enumeration constants, and each constant has an associated integer value, starting from 0 by default. 

Enumerated types provide a way to define a variable that can only take one of a specified set of values, making the code more readable and type-safe.

**Syntax**

```pascal linenums="1"
type
  TEnumName = (Value1, Value2, Value3, ...);
```

**Example**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

type
  TDay = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

var
  today: TDay;

  // Main block
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
end.
```

**Example of an Enum in case statement**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

type
  TColor = (Red, Green, Blue);

var
  color: TColor;

  // Main block
begin
  color := Green;
  case color of
    Red: WriteLn('Red');
    Green: WriteLn('Green');
    Blue: WriteLn('Blue');
  end;
end.
```

## 17. Subrange Types

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

  // Main block
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
    
    This (1) helps prevent errors and makes your code more robust and (2) readable. For example, if you accidentally try to assign a value outside the range, the compiler will catch the error.

## 18. Arrays

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
var
  numbers: array[1..5] of Integer;

  // Main block
begin
  numbers[1] := 10;  // Set the first element to 10
  numbers[2] := 20;  // Set the second element to 20
  WriteLn(numbers[1]); // This will print 10
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

var
  numbers: array[1..5] of integer;
  i: integer;

  // Main block
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

var
  numbers: array of integer;
  i: integer;

  // Main block
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

  // Main block
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

procedure PrintArray(arr: array of integer);
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    WriteLn('arr[', i, '] = ', arr[i]);
end;

var
  numbers: array[1..5] of integer;

  // Main block
begin
  // Initialising the array
  numbers[1] := 10;
  numbers[2] := 20;
  numbers[3] := 30;
  numbers[4] := 40;
  numbers[5] := 50;

  // Passing the array to the procedure
  PrintArray(numbers);
end.
```


## 19. Records Types

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
  {$mode objfpc}{$H+}{$J-}

type
  TPerson = record
    name: string;
    age: integer;
    height: real;
  end;

var
  person1, person2: TPerson;

  // Main block
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
end.
```

## 20. Advanced Records

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
```

We use the `TRectangle` like this:

```pascal linenums="1"
var
  Rect: TRectangle;
  
  {Main Block}
begin
  // Create a rectangle with width 10 and height 5
  Rect := TRectangle.Create(10, 5);
  
  // Show the details of the rectangle
  Rect.ShowDetails;
end.
```

**Full Example**

```pascal linenums="1"
program AdvancedRecordDemo;

{$mode objfpc}{$H+}{$J-}

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

  // Main block
begin
  // Create a rectangle with width 10 and height 5
  Rect := TRectangle.Create(10, 5);
  
  // Show the details of the rectangle
  Rect.ShowDetails;
end.
```

## 21. Classes

Here is a simple example of creating a class. For mroe info, visit the official documentation; [Classes](https://www.freepascal.org/docs-html/ref/refch6.html#x69-930006). 

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

  // Main block
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

## 22. Generics

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

// Main block
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

// Main block
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

  // Main block
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


## 23. Function References

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

// Main block
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

**Example - A Calculator**

```pascal linenums="1"
program FuncRefCalculatorExample;

{$mode objfpc}{$H+}{$J-}
{$modeswitch functionreferences}

uses
  SysUtils;

type
  // Define a type represents a reference to a function
  // that takes two doubles and returns a double
  TMathOperation = reference to function(a, b: double): double;

  // Define basic math operations
  function Add(a, b: double): double;
  begin
    // Return the sum of a and b
    Result := a + b;
  end;

  function Subtract(a, b: double): double;
  begin
    // Return the difference of a and b
    Result := a - b;
  end;

  function Multiply(a, b: double): double;
  begin
    // Return the product of a and b
    Result := a * b;
  end;

  function Divide(a, b: double): double;
  begin
    try
      // Return the quotient of a and b
      Result := a / b
    except
      on E: Exception do
        WriteLn('Error: ' + E.Message);
    end;
  end;

  // Procedure to perform a math operation
  // Takes a math operation and two operands as arguments
  procedure Calculate(mathOp: TMathOperation; a, b: double);
  begin
    WriteLn(mathOp(a, b): 0: 2);
  end;

// Main block
begin
  Calculate(@Add, 10, 5);      // Outputs: 15.00
  Calculate(@Subtract, 10, 5); // Outputs: 5.00
  Calculate(@Multiply, 10, 5); // Outputs: 50.00
  Calculate(@Divide, 10, 5);   // Outputs: 2.00

  // Pause console
  WriteLn('Press Enter to quit');
  ReadLn;
end.
```

**Example - Digital Circuit Simulator**

```pascal linenums="1"
program FuncRefDigitalCircuitSimulator;

{$mode objfpc}{$H+}{$J-}
{$modeswitch functionreferences}

uses
  SysUtils;

type
  // Define a function reference type that takes booleans and return a boolean
  TLogicGate = reference to function(A, B: boolean): boolean;

  // Define the AND logic gate function
  function ANDGate(A, B: boolean): boolean;
  begin
    Result := A and B;
  end;

  // Define the OR logic gate function
  function ORGate(A, B: boolean): boolean;
  begin
    Result := A or B;
  end;

  // Define the XOR logic gate function
  function XORGate(A, B: boolean): boolean;
  begin
    Result := A xor B;
  end;

  // Procedure to simulate a digital circuit using a given logic gate
  procedure SimulateCircuit(Gate: TLogicGate; A, B: boolean);
  begin
    // Print the result of applying the logic gate to A and B
    WriteLn(BoolToStr(Gate(A, B), True));
  end;

begin
  // Simulate the circuit with the AND gate and inputs True and False
  SimulateCircuit(@ANDGate, True, False);  // Outputs: FALSE

  // Simulate the circuit with the OR gate and inputs True and False
  SimulateCircuit(@ORGate, True, False);   // Outputs: TRUE

  // Simulate the circuit with the XOR gate and inputs True and False
  SimulateCircuit(@XORGate, True, False);  // Outputs: TRUE

  // Pause the console to view the output
  WriteLn('Press Enter to quit');
  ReadLn;
end.
```

## 24. Anonymous Functions

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

**Example - Grade Calculator**

```pascal linenums="1"
program AnonymousFuncGradeCalculator;

{$mode objfpc}{$H+}{$J-}
{$modeswitch anonymousfunctions}  // Enable anonymous functions

var
  calculateGrade: function(score: integer): string;

begin
  // Assigning an anonymous function to the 'CalculateGrade' variable
  calculateGrade := function(score: integer): string
  begin
    if score >= 85 then
      Result := 'HD'
    else if score >= 75 then
      Result := 'D'
    else if score >= 65 then
      Result := 'C'
    else if score >= 50 then
      Result := 'P'
    else
      Result := 'F';
  end;

  // Using the anonymous function to calculate grades
  Writeln('Grade for score 90: ', CalculateGrade(90));
  Writeln('Grade for score 70: ', CalculateGrade(70));
  Writeln('Grade for score 40: ', CalculateGrade(40));

  // Pause the console to view the output
  WriteLn('Press Enter to quit');
  ReadLn;
end.
```


**Example - Student Data Processor**

```pascal linenums="1"
program StudentDataProcessor;

{$mode objfpc}{$H+}{$J-}
{$modeswitch anonymousfunctions}// Enable anonymous functions

type
  TStudent = record
    Name: string;
    Age: integer;
    Score: integer;
  end;

type
  TStudentList = array of TStudent;
  TStudentPredicate = function(student: TStudent): boolean;

var
  studentList: TStudentList;
  student: TStudent;
  filterStudents: function(arr: TStudentList; condition: TStudentPredicate): TStudentList;

begin
  // Initialize student data
  SetLength(studentList, 5);

  // Populate student data
  studentList[0].Name := 'Brent';
  studentList[0].Age := 20;
  studentList[0].Score := 85;
  studentList[1].Name := 'Dylan';
  studentList[1].Age := 22;
  studentList[1].Score := 90;
  studentList[2].Name := 'Jared';
  studentList[2].Age := 21;
  studentList[2].Score := 78;
  studentList[3].Name := 'Holly';
  studentList[3].Age := 20;
  studentList[3].Score := 92;
  studentList[4].Name := 'Julie';
  studentList[4].Age := 23;
  studentList[4].Score := 88;

  // Filters a list of students based on a given condition.
  // Returns a new list containing only the students that satisfy the condition.
  filterStudents := function(arr: TStudentList; condition: TStudentPredicate): TStudentList
  var
    resultArr: array of TStudent;
    i: integer;
  begin
    SetLength(resultArr, 0);
    for i := Low(arr) to High(arr) do if condition(arr[i]) then
      begin
        SetLength(resultArr, Length(resultArr) + 1);
        resultArr[High(resultArr)] := arr[i];
      end;
    Result := resultArr;
  end;


  // Using the anonymous function to filter studentList based on conditions
  Writeln('Students with score above 85:');
  for student in filterStudents(studentList, function(student: TStudent): boolean
                                             begin
                                               Result := student.Score > 85;
                                             end) do
    Writeln(student.Name, ' - ', student.Score);

  Writeln('Students aged 21 or below:');
  for student in filterStudents(studentList, function(student: TStudent): boolean
                                             begin
                                               Result := student.Age <= 21;
                                             end) do
    Writeln(student.Name, ' - ', student.Age);

  // Pause the console to view the output
  WriteLn('Press Enter to quit');
  ReadLn;
end.
```

## 25. Interfaces

!!! Note
    By default, Free Pascal uses the Windows COM `IUnknown` interface type.

    Ref: [$INTERFACES : Specify Interface type.](https://www.freepascal.org/docs-html/current/prog/progsu37.html#x44-430001.2.37)

Think of an interface as a plan that outlines what actions a class should perform, without specifying how to do them. In Free Pascal, interfaces serve as an alternative to multiple inheritance, which is used in languages like C++.

- Interfaces can only be used in `delphi` or `objfpc` modes. 
- All parts of an `interface` are always `public`, so you can't hide them.
- Properties can only have methods to get or set their values. 
- You can't create interfaces directly. Instead, you need a `class` that uses the `interface`.
- It is not possible for a class to implement only part of the interface: it is all or nothing.
- You can only use calling convention modifiers in methods within an interface. You can't use special modifiers like `virtual`, `abstract`, `dynamic`, or `override` in an `interface`.

!!! Info
    Refer to the official doc [Interfaces](https://www.freepascal.org/docs-html/ref/refch7.html#x96-1200007) for more info.

**Syntax**

1. Use the `interface` keyword to define an interface, specifying the methods (and properties, if any) that any implementing class must provide.

2. Use the `class` keyword to define a class that implements the interface. The class **must provide concrete implementations** for all the methods and properties declared in the interface.

**Example**

This example defines a simple interface `IMyInterface` with one method `DoSomething`, and then implement this interface in a class `TMyClass`.

**1. Define the Interface**

!!! Note
    Especially on Windows systems, the GUID of an interface can and **must be used** when using COM.

    Source: [Interfaces - Definition](https://www.freepascal.org/docs-html/ref/refse44.html#x97-1210007.1)

```pascal linenums="1"
type
  IMyInterface = interface
    ['{12345678-1234-1234-1234-1234567890AB}'] // Unique identifier (GUID) for the interface
    procedure DoSomething;
  end;
```

**Step 2: Implement the Interface in a Class**

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

**Step 3: Use the Interface and Class**

```pascal linenums="1"
var
  MyObject: IMyInterface;
begin
  MyObject := TMyClass.Create;
  MyObject.DoSomething;
end.
```

You don’t need to call `MyObject.Free` because COM interfaces automatically handle memory management. When no more references to the COM interface exist (when the reference count reaches zero), the object that implements the interface is automatically freed.

!!! Note
    All COM interfaces use reference counting. This means that whenever an interface is assigned to a variable, its reference count is updated. Whenever the variable goes out of scope, the reference count is automatically decreased. 
    
    When the reference count reaches zero, usually the instance of the class that implements the interface, is freed.

    Source: [Interfaces - Reference counting](https://www.freepascal.org/docs-html/current/ref/refse51.html#x104-1280007.8)

!!! Note
    ... COM interfaces are by default reference counted, because they descend from `IUnknown`.

    Source: [Interfaces - CORBA and other interfaces](https://www.freepascal.org/docs-html/ref/refse50.html)

**Complete Example**

```pascal linenums="1"
program COMInterfaceExample;

{$mode objfpc}{$H+}{$J-}

type
  // Step 1: Define the Interface
  IMyInterface = interface
    ['{12345678-1234-1234-1234-1234567890AB}'] // Unique identifier (GUID) for the interface
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

- The GUID `['{12345678-1234-1234-1234-1234567890AB}']` is required for COM compatibility but can be a unique identifier in your application.
- `TInterfacedObject` is a base class that implements `IUnknown`, which is the ancestor of all interfaces. This ensures proper reference counting for memory management.

## 26. More on COM Interfaces

### What is a GUID?

A GUID is like a super-unique name tag. Imagine you're at a huge event with thousands of people, and everyone needs to wear a name tag to avoid confusion. Each name tag has to be unique so that when someone calls out a name, only one person responds. That's what a GUID does for interfaces in programming.

### Why do we need a GUID for interfaces?

When we create interfaces in programming, we often have many different interfaces that might look similar. The GUID helps us keep track of each one and makes sure there's no mix-up. Here’s why this is important:

1. **Uniqueness**: Just like a unique name tag, a GUID makes sure that each interface is uniquely identified. No two interfaces will have the same GUID.

2. **Identification**: When your program is running, it might need to check if an object (a piece of data or a function) follows a certain set of rules (an interface). The GUID is used to ask,*"Do you follow these rules?"* and get a clear answer.

3. **Compatibility**: In complex programs or systems that involve many parts working together, like different pieces of software communicating with each other, the GUID ensures that they all understand each other correctly. It's like a universal language for interfaces.

### Example to Understand GUID

Imagine you're organizing a science fair. Each project needs a unique ID so judges know exactly which project they're looking at. Without unique IDs, two projects could have the same name, leading to confusion. GUIDs work the same way for interfaces in programming.

### Practical Example in Programming

Here's a simple example in Free Pascal:

1. Define the Interface with a GUID

```pascal linenums="1"
type
  IMyInterface = interface
    ['{12345678-1234-1234-1234-1234567890AB}'] // This is the GUID
    procedure DoSomething;
  end;
```

2. Implement the Interface in a Class

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

3. Use the Interface and Class

```pascal linenums="1"
var
  MyObject: IMyInterface;
begin
  MyObject := TMyClass.Create;
  MyObject.DoSomething;
end.
```

### Breaking Down the Example

- **Define the Interface**: `IMyInterface` is like a rulebook that says any class that follows it must have a `DoSomething` procedure.
- **GUID**: `{12345678-1234-1234-1234-1234567890AB}` is a unique identifier for IMyInterface. It's like saying, *"This rulebook has a unique ID so there's no confusion."*
- **Implement the Interface**: `TMyClass` says, *"I follow the IMyInterface rulebook and provide a DoSomething procedure."*
- **Using the Interface**: The program creates an instance of `TMyClass` and calls `DoSomething` on it, knowing exactly which rules it's following because of the GUID.

### Summary

- GUIDs are unique identifiers that ensure interfaces are uniquely recognized.
- They help prevent confusion in large and complex systems.
- They allow programs to check if objects follow specific rules (interfaces) correctly.
- Think of a GUID as a unique fingerprint for an interface, ensuring it’s always identified correctly and uniquely in a program.


## 27. Even More on COM Interfaces


### What is a Function?

Think of a function as a recipe. If you have a recipe for chocolate chip cookies, you follow those instructions every time you want cookies. You don’t need to worry about the recipe being mixed up with other recipes because you have the name of the recipe right there.

### What is an Interface?

An interface is like a contract or a blueprint that tells different objects (think of them as different people or tools) how they should behave. For instance, imagine you have a blueprint for different types of devices that can play music, like a smartphone, a tablet, or a speaker. Each of these devices follows the same set of instructions (the interface) for how to play music, but they might play it differently.

### Why Does an Interface Need a GUID?

**Unique Identification:**

- **Functions**: In a program, you call functions by their names. If you want to bake cookies, you just call the "cookie recipe" function. There's no need for a special identifier because each function name is unique within its context.
- **Interfaces**: Different interfaces might have similar methods, but they need a way to be uniquely identified. This is because many objects (devices) can follow the same interface (blueprint). The GUID acts like a unique serial number to make sure you’re dealing with the exact right blueprint.

**Multiple Implementations:**

- **Functions**: Each function is a specific set of instructions in your code. If you call a function, you're calling a specific set of instructions.
- **Interfaces**: An interface can be implemented by many different classes (objects). For example, you could have a `Player` interface for different types of media players. Each player (smartphone, tablet, speaker) will follow the same `Player` interface but might have different ways of playing the music. The GUID helps ensure that when you ask for a `Player`, you get the right kind of `Player`.

**Checking at Runtime:**

- **Functions**: When your program runs, it directly calls functions by their names. No extra checking is needed because you know exactly what function you're calling.
- **Interfaces**: Sometimes, you need to check if an object follows a particular interface, especially if you’re not sure what kind of object you have. The GUID helps you confirm that the object adheres to the right blueprint.

### Simple Analogy

Imagine you're at a huge convention where every booth has a unique ID number. Each booth might have a different type of product, but the unique ID ensures that when you ask for a specific type of product, you find the right booth.

- **Functions**: Like knowing the exact name of a recipe.
- **Interfaces**: Like having a unique ID for each type of product to make sure you get the right one.

### Example in Programming

Let's say you have an interface called `IDriveable` that any vehicle (like cars or bikes) should implement.

Interface Definition:

```pascal linenums="1"
type
  IDriveable = interface
    ['{11111111-1111-1111-1111-111111111111}'] // Unique ID
    procedure Drive;
  end;
```

Class Implementing the Interface:

```pascal linenums="1"
type
  TCar = class(TInterfacedObject, IDriveable)
  public
    procedure Drive;
  end;

procedure TCar.Drive;
begin
  WriteLn('Driving a car...');
end;
```

Using the Interface:

```pascal linenums="1"
var
  Vehicle: IDriveable;
  MyCar: TCar;
begin
  MyCar := TCar.Create;
  Vehicle := MyCar;
  Vehicle.Drive; // Calls the Drive method from TCar
end.
```

In this example, `IDriveable` has a unique `GUID`, so even if you have many different classes (like `TCar`, `TBike`) that implement IDriveable, the GUID ensures you’re interacting with the right interface.

### Summary

- **GUID for Interfaces**: Ensures each interface is uniquely identified, especially when dealing with multiple implementations.
- **Ordinary Functions**: Are unique by their names within their code context, so they don’t need an extra unique identifier.

The GUID is like a special label that makes sure you’re talking to the exact right set of instructions (interface) among many possibilities.

## 28. Pointers

> ... Avoid pointer whenever alternatives exist. If you want to learn, though, there's no silver bullet apart from: There has to be as many `Dispose` as `New`, period. 
> 
> Source: [Leledumbo's reply on 'Dispose of Pointer', 2023-08-10](https://forum.lazarus.freepascal.org/index.php/topic,64238.msg487999.html#msg487999).
 
**Example**

```pascal linenums="1"
program PointerExample;

var
  ptr: ^integer;  // Declare a pointer to Integer
  value: integer;

begin
  New(ptr);           // Allocate memory for an Integer
  ptr^ := 42;         // Assign value 42 to the memory location
  value := ptr^;      // Access the value through the pointer

  Writeln('Value pointed to by ptr: ', value);

  Dispose(ptr);       // Free the allocated memory
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