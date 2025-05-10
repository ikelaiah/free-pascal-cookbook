# Common Data Types in Free Pascal

Here is a list of common data types in Free Pascal, along with a simple example.

See the official docs for more info; [Types](https://www.freepascal.org/docs-html/ref/refch3.html#x24-230003).

## Summary Table

| **Category** | **Keyword / Type** | **Range / Size** |
| --- | --- | --- |
| Integer | `Byte` | 0 .. 255 |
| Integer | `ShortInt` | \-128 .. 127 |
| Integer | `SmallInt` | \-32,768 .. 32,767 |
| Integer | `Integer` | \-2,147,483,648 .. 2,147,483,647 (typically 32-bit) |
| Integer | `LongInt` | Same as `Integer` (typically 32-bit) |
| Integer | `Int64` | \-9,223,372,036,854,775,808 .. 9,223,372,036,854,775,807 (64-bit) |
| Integer | `Word` | 0 .. 65,535 (unsigned 16-bit) |
| Integer | `Cardinal` | 0 .. 4,294,967,295 (unsigned 32-bit, same as `LongWord`) |
| Integer | `QWord` | 0 .. 18,446,744,073,709,551,615 (unsigned 64-bit) |
| Boolean | `Boolean` | `True` or `False` |
| Character | `Char` | Single ASCII/ANSI character (or UTF-8 character element) |
| String | `String` | Sequence of characters (modern FPC: typically dynamic, like `AnsiString` or `UnicodeString`) |
| Floating-Point | `Single` | Approx. ±1.5 x 10^−45 .. ±3.4 x 10^38 (7-8 significant digits) |
| Floating-Point | `Real` | Often an alias for `Double` in modern FPC. Historically platform-dependent. |
| Floating-Point | `Double` | Approx. ±5.0 x 10^−324 .. ±1.7 x 10^308 (15-16 significant digits) |
| Floating-Point | `Extended` | Higher precision than `Double` (platform-dependent, often 80-bit) |
| Enumerated | User-defined | A set of named constants |
| Subrange | User-defined | A specific range of an ordinal type |
| Record | `record` | A collection of fields, grouping different data types |
| Advanced Record | `record helper` or `record` with methods | Records with associated procedures and functions |
| Array | `array` | Fixed-size or dynamic list of elements of the same type |
| Pointer | `^TypeName` or `Pointer` | Memory address of a variable or data structure |

The following examples are complete programs. You can copy, paste, and run them to see the data types in action. Remember to save files with a `.pas` extension.

## Integer Types

### Byte
    
- **Range:** 0 .. 255
- **Example:**
        
```pascal linenums="1"
program ExampleByte;
{$mode objfpc}{$H+}{$J-}

var
  b: Byte; 
begin   
  b := 100;   
  WriteLn('Byte value: ', b); 
  ReadLn;
end.
```
        
### ShortInt
    
- **Range:** -128 .. 127
- **Example:**
        
```pascal linenums="1"
program ExampleShortInt;
{$mode objfpc}{$H+}{$J-}

var   
  s: ShortInt; 
begin   
  s := -50;   
  WriteLn('ShortInt value: ', s); 
  ReadLn;
end.
```
        
### SmallInt
    
- **Range:** -32,768 .. 32,767
- **Example:**
        
```pascal linenums="1"
program ExampleSmallInt;
{$mode objfpc}{$H+}{$J-}

var
  sm: SmallInt;
begin
  sm := 32000;
  WriteLn('SmallInt value: ', sm);
  ReadLn;
end.
```
        
### Integer
    
- **Range:** Typically -2,147,483,648 .. 2,147,483,647 (32-bit signed)
- **Example:**
        
```pascal linenums="1"
program ExampleInteger;
{$mode objfpc}{$H+}{$J-}

var
  i: Integer;
begin
  i := 1234567890;
  WriteLn('Integer value: ', i);
  ReadLn;
end.
```

### LongInt
    
- **Range:** Same as `Integer` (typically 32-bit signed)
- **Example:**
        
```pascal linenums="1"
program ExampleLongInt;
{$mode objfpc}{$H+}{$J-}

var
  li: LongInt;
begin
  li := -2147483648;
  WriteLn('LongInt value: ', li);
  ReadLn;
end.
```

### Int64
    
- **Range:** -9,223,372,036,854,775,808 .. 9,223,372,036,854,775,807 (64-bit signed)
- **Example:**
        
```pascal linenums="1"
program ExampleInt64;
{$mode objfpc}{$H+}{$J-}

var
  i64: Int64;
begin
  i64 := 9223372036854775807;
  WriteLn('Int64 value: ', i64);
  ReadLn;
end.
```

### Word
    
- **Range:** 0 .. 65,535 (16-bit unsigned)
- **Example:**
        
```pascal linenums="1"
program ExampleWord;
{$mode objfpc}{$H+}{$J-}

var
  w: Word;
begin
  w := 65535;
  WriteLn('Word value: ', w);
  ReadLn;
end.
```

### Cardinal
    
- **Range:** 0 .. 4,294,967,295 (32-bit unsigned, same as `LongWord`)
- **Example:**
        
```pascal linenums="1"
program ExampleCardinal;
{$mode objfpc}{$H+}{$J-}

var
  c: Cardinal;
begin
  c := 4294967295;
  WriteLn('Cardinal value: ', c);
  ReadLn;
end.
```

### QWord
    
- **Range:** 0 .. 18,446,744,073,709,551,615 (64-bit unsigned)
- **Example:**
        
```pascal linenums="1"
program ExampleQWord;
{$mode objfpc}{$H+}{$J-}

var
  qw: QWord;
begin
  qw := 18446744073709551615;
  WriteLn('QWord value: ', qw);
  ReadLn;
end.
```


## Boolean

- **Values:** `True` or `False`
- **Example:**
        
```pascal linenums="1"
program ExampleBoolean;
{$mode objfpc}{$H+}{$J-}

var
  isValid: Boolean;
  isComplete: Boolean;
begin
  isValid := True;
  isComplete := False;
  WriteLn('isValid: ', isValid);       // Output: TRUE
  WriteLn('isComplete: ', isComplete); // Output: FALSE
  ReadLn;
end.
```


## Char (Character)

- **Range:** Typically a single ASCII or ANSI character. In UTF-8 mode, it can be one byte of a multi-byte character.
- **Example:**
        
```pascal linenums="1"
program ExampleChar;
{$mode objfpc}{$H+}{$J-}

var
  letter: Char;
  symbol: Char;
begin
  letter := 'A';
  symbol := '#';
  WriteLn('Letter: ', letter);
  WriteLn('Symbol: ', symbol);
  ReadLn;
end.
```


## String

- **Description:** A sequence of characters. In modern Free Pascal, `String` is typically dynamic (like `AnsiString` or `UnicodeString` depending on compiler settings), meaning its length can change. The older `ShortString` type has a fixed maximum length of 255 characters.
- **Example:**
        
```pascal linenums="1"
program ExampleString;
{$mode objfpc}{$H+}{$J-}

var
  greeting: String;
begin
  greeting := 'Hello, Pascal World!';
  WriteLn(greeting);
  WriteLn('Length of greeting: ', Length(greeting));
  ReadLn;
end.
```


## Floating-Point Types

### Single
    
- **Range:** Approximately ±1.5 x 10^−45 .. ±3.4 x 10^38 (about 7-8 significant decimal digits)
- **Example:**
        
```pascal linenums="1"
program ExampleSingle;
{$mode objfpc}{$H+}{$J-}

var
  s: Single;
begin
  s := 3.1415926535;
  WriteLn('Single value: ', s); // Output will be an approximation
  ReadLn;
end.
```

### Real
    
- **Description:** In modern Free Pascal, `Real` is often an alias for `Double`. Historically, its size and precision were platform-dependent.
- **Example:**
        
```pascal linenums="1"
program ExampleReal;
{$mode objfpc}{$H+}{$J-}

var
  r: Real;
begin
  r := 2.718281828459045;
  WriteLn('Real value: ', r); // Output will be an approximation, likely with Double precision
  ReadLn;
end.
```

### Double
    
- **Range:** Approximately ±5.0 x 10^−324 .. ±1.7 x 10^308 (about 15-16 significant decimal digits)
- **Example:**


```pascal linenums="1"
program ExampleDouble;
{$mode objfpc}{$H+}{$J-}

var
  d: Double;
begin
  d := 1.7976931348623157E+308;
  WriteLn('Double value: ', d);
  ReadLn;
end.
```


### Extended
    
- **Description:** Offers higher precision than `Double`. Its exact size and precision are platform-dependent (often 80-bit on x86 systems).
- **Example:**
        
```pascal linenums="1"
program ExampleExtended;
{$mode objfpc}{$H+}{$J-}

var
  e: Extended;
begin
  e := 1.234567890123456789E+4932; // Example of a very large number
  WriteLn('Extended value: ', e);
  ReadLn;
end.
```        

## Enumerated Types

- **Description:** A type where you define a set of named values. Internally, these are represented by integers (0, 1, 2,...).
- **Example:**
    
```pascal linenums="1"
program ExampleEnum;
{$mode objfpc}{$H+}{$J-}

type
  TColor = (Red, Green, Blue, Yellow);
var
  myColor: TColor;
begin
  myColor := Green;
  // WriteLn directly prints the ordinal value of the enum
  WriteLn('My color (ordinal value): ', Ord(myColor)); // Output: 1 (Green is the second item, ordinal 1)
  
  // To print the name, you might use a case statement or RTTI (more advanced)
  case myColor of
    Red: WriteLn('The color is Red');
    Green: WriteLn('The color is Green');
    Blue: WriteLn('The color is Blue');
    Yellow: WriteLn('The color is Yellow');
  end;
  ReadLn;
end.
```

## Subrange Types

- **Description:** A type that represents a subset of values from another ordinal type (like Integer or Char).
- **Example:**
    
```pascal linenums="1"
program ExampleSubrange;
{$mode objfpc}{$H+}{$J-}

type
  TScore = 0..100;
  TUpperCaseLetter = 'A'..'Z';
var
  myScore: TScore;
  firstInitial: TUpperCaseLetter;
begin
  myScore := 85;
  firstInitial := 'P';
  WriteLn('My score: ', myScore);
  WriteLn('First initial: ', firstInitial);

  // myScore := 101; // This would cause a runtime error if range checking is on
  ReadLn;
end.
```    

## Record Types

- **Description:** A composite data type that groups together variables (fields) of different types under a single name.
- **Example:**
    
```pascal linenums="1"
program ExampleRecord;
{$mode objfpc}{$H+}{$J-}

type
  TPerson = record
    Name: String[50]; // ShortString for simplicity in basic record
    Age: Integer;
  end;
var
  person: TPerson;
begin
  person.Name := 'Alice Wonderland';
  person.Age := 30;
  WriteLn(person.Name, ' is ', person.Age, ' years old.');
  ReadLn;
end.
```    

## Advanced Records (with methods)

- **Description:** Records in modern Object Pascal can also have methods (procedures and functions) associated with them, similar to objects.
- **Example:**
    
```pascal linenums="1"
program ExampleAdvancedRecord;
{$mode objfpc}{$H+}{$J-}

type
  TAdvPerson = record
    Name: String[50];
    Age: Integer;
    procedure DisplayInfo; // Method declaration
  end;

// Implementation of the method
procedure TAdvPerson.DisplayInfo;
begin
  WriteLn(Name, ' is ', Age, ' years old. (From record method)');
end;

var
  advPerson: TAdvPerson;
begin
  advPerson.Name := 'Bob The Builder';
  advPerson.Age := 40;
  advPerson.DisplayInfo; // Calling the method
  ReadLn;
end.    
```

## Arrays

### Static Array
    
- **Description:** An array with a fixed size determined at compile time.
- **Example:**
        
```pascal linenums="1"
program ExampleStaticArray;
{$mode objfpc}{$H+}{$J-}

var   
  scores: array[1..5] of Integer; // Array of 5 integers, indexed 1 to 5
  i: Integer;
begin   
  scores[1] := 100;
  scores[2] := 90;
  scores[3] := 85;
  scores[4] := 70;
  scores[5] := 95;

  WriteLn('Scores:');
  for i := 1 to 5 do
    WriteLn('Score #', i, ': ', scores[i]);
  ReadLn;
end.
```

### Dynamic Array
    
- **Description:** An array whose size can be changed at runtime using `SetLength`. Dynamic arrays are 0-indexed.
- **Example:**
        
```pascal linenums="1"
program ExampleDynamicArray;
{$mode objfpc}{$H+}{$J-}

var
  names: array of String; // Dynamic array of strings
  i: Integer;
begin
  SetLength(names, 3); // Allocate space for 3 strings

  names[0] := 'Alice';
  names[1] := 'Bob';
  names[2] := 'Charlie';

  WriteLn('Names:');
  for i := 0 to High(names) do // High(names) gives the last valid index (Length - 1)
    WriteLn(names[i]);
  
  SetLength(names, 2); // Resize the array
  names[0] := 'David';
  names[1] := 'Eve';

  WriteLn('Resized Names:');
  for i := 0 to High(names) do
    WriteLn(names[i]);
  ReadLn;
end.
```        

## Pointers

- **Description:** A pointer holds the memory address of a variable. They are used for dynamic memory allocation and low-level programming. `^TypeName` declares a pointer to `TypeName`. `Pointer` is a generic pointer type.
- **Example:**
    
```pascal linenums="1"
program ExamplePointer;
{$mode objfpc}{$H+}{$J-}

var
  pNum: ^Integer; // pNum is a pointer to an Integer
  num: Integer;
begin   
  // Allocate memory for an Integer and make pNum point to it
  New(pNum); 
  
  pNum^ := 123; // Assign a value to the memory location pNum points to (dereferencing)
  
  WriteLn('Value via pointer: ', pNum^);
  
  num := pNum^; // Assign the pointed-to value to another variable
  WriteLn('Value in num variable: ', num);
  
  // Release the allocated memory
  Dispose(pNum); 
  pNum := nil; // Good practice to set pointer to nil after disposing

  ReadLn; 
end. 
```