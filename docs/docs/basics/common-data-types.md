# Common Data Types in Free Pascal

## Understanding Data Types

Every piece of data your program handles needs a type. A data type tells the compiler what kind of value you're storing—a whole number, a decimal, text, true/false, and so on. Different types use different amounts of memory and have different limitations.

**Why do we need different types?** Different types are designed for different purposes. A `Byte` uses 1 byte of memory and can store 0-255, perfect for small counts. An `Integer` uses 4 bytes and can store much larger numbers. If you only need to count up to 255, `Byte` is more efficient. For decimal numbers, you'd use `Single` or `Double` instead of integers.

Here is a reference of common data types in Free Pascal, along with simple examples you can try yourself.

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
| Floating-Point | `Single` | Approx. ±1.5 x 10^−45 .. ±3.4 x 10^38 (about 7 accurate decimal places) |
| Floating-Point | `Real` | Often an alias for `Double` in modern FPC. Historically platform-dependent. |
| Floating-Point | `Double` | Approx. ±5.0 x 10^−324 .. ±1.7 x 10^308 (about 15 accurate decimal places) |
| Floating-Point | `Extended` | Higher precision than `Double` (platform-dependent, often 80-bit) |
| Enumerated | User-defined | A set of named constants |
| Subrange | User-defined | A specific range of an ordinal type |
| Record | `record` | A collection of fields, grouping different data types |
| Advanced Record | `record helper` or `record` with methods | Records with associated procedures and functions |
| Array | `array` | Fixed-size or dynamic list of elements of the same type |
| Pointer | `^TypeName` or `Pointer` | Memory address of a variable or data structure |

The following examples are complete programs. You can copy, paste, and run them to see the data types in action. Remember to save files with a `.pas` extension.

## Quick Reference: Which Type Should I Use?

| **What you're storing** | **Use this type** | **Why** |
| --- | --- | --- |
| Small whole numbers (0-255) | `Byte` | Uses less memory, fast |
| Small whole numbers (negative or positive) | `ShortInt` | Uses 1 byte |
| Normal whole numbers | `Integer` | Good for most situations |
| Very large whole numbers | `Int64` | Up to 9 billion and beyond |
| Decimal numbers (like 3.14) | `Double` | More accurate than `Single` |
| Text/words | `String` | Can store any text |
| True or false only | `Boolean` | For yes/no decisions |

---

## Integer Types

**What are integers?** Integers are whole numbers without decimals (like 5, -10, 1000). Different integer types can store different ranges of numbers and use different amounts of memory.

**A note about "accurate decimal places"**: When we say `Single` has "7 accurate decimal places," it means if you store a number like 3.1415926, you can reliably trust the first 7 digits. The 8th digit and beyond might not be exact. `Double` is more precise—it can reliably store about 15 decimal places.

### Byte

- **Range:** 0 .. 255 (unsigned, meaning no negative numbers)
- **When to use:** Ages, test scores (0-100), array indices, or any small positive count
- **Memory:** 1 byte
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

- **Range:** -128 .. 127 (signed, can be negative or positive)
- **When to use:** Temperature readings, altitude differences, or small numbers that might be negative
- **Memory:** 1 byte
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
- **When to use:** Most everyday counting (user IDs, scores, populations). This is the "default" integer type and a good choice when you're unsure
- **Memory:** 4 bytes
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

- **Range:** -9,223,372,036,854,775,808 .. 9,223,372,036,854,775,807 (64-bit signed—huge numbers!)
- **When to use:** Very large numbers like national populations, file sizes in bytes, or milliseconds since a date
- **Memory:** 8 bytes
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

- **Description:** A sequence of characters (text). In modern Free Pascal, `String` is dynamic, meaning it grows and shrinks as needed. You can store "Hi" or "This is a very long sentence" in the same variable.
- **ShortString (old style):** You might see `String[50]` in old code—this is a ShortString with a maximum of 50 characters. Avoid this in new code; use plain `String` instead.
- **When to use:** Storing names, messages, user input—basically any text data
- **Memory:** Dynamic (grows as needed)
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

**What are floating-point numbers?** These are numbers with decimals (like 3.14, -0.5, or 2.718). They're called "floating-point" because the decimal point can "float" to different positions. Important note: floating-point numbers are approximations, not exact values. Don't use them for money calculations unless you know what you're doing!

### Single

- **Range:** Approximately ±1.5 x 10^−45 .. ±3.4 x 10^38
- **Precision:** About 7 accurate decimal places (e.g., 3.141592 is reliable, but the 8th digit might be wrong)
- **When to use:** Graphics, physics simulations, or when memory matters more than perfect accuracy
- **Memory:** 4 bytes
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

- **Range:** Approximately ±5.0 x 10^−324 .. ±1.7 x 10^308
- **Precision:** About 15 accurate decimal places (much better than `Single`)
- **When to use:** Science, engineering, or any calculation where accuracy matters. `Double` is usually the best choice for decimal numbers
- **Memory:** 8 bytes
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

- **Description:** Offers higher precision than `Double`. Its exact size is platform-dependent (often 80-bit on x86 systems). Use only if you need extreme precision.
- **When to use:** Advanced scientific calculations requiring extreme precision
- **Memory:** Platform-dependent (typically 10 bytes)
- **Example:**

```pascal linenums="1"
program ExampleExtended;
{$mode objfpc}{$H+}{$J-}

var
  e: Extended;
begin
  e := 1.234567890123456789E+308; // Example with very high precision
  WriteLn('Extended value: ', e:0:15);  // Format to show 15 decimal places
  ReadLn;
end.
```        

## Enumerated Types

- **Description:** A type where you define a set of named values. Internally, these are represented by integers (0, 1, 2,...).
- **When to use:** When you have a fixed list of options (like colors, days of the week, or game states). Much clearer than using magic numbers!
- **Example use case:** Instead of storing day as 1, 2, 3... (confusing), use an enum with names: Monday, Tuesday, Wednesday (clear and self-documenting)
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

- **Description:** A type that represents a subset of values from another ordinal type (like Integer or Char). For example, instead of allowing any Integer (which includes negatives), you might define a type that only allows 0-100.
- **When to use:** When you want to limit a variable to a specific range. The compiler can catch errors if you try to assign invalid values!
- **Example use cases:**
  - Test scores: 0..100 (not negative, not over 100)
  - Month numbers: 1..12 (only 12 months)
  - Grade letters: 'A'..'F' (only uppercase letters A through F)
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
    Name: String;  // Use modern dynamic String, not ShortString
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
- **Note:** Advanced records require a special compiler directive to work (see below).
- **Example:**

```pascal linenums="1"
program ExampleAdvancedRecord;
{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

type
  TAdvPerson = record
    Name: String;  // Modern dynamic String
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

!!! warning "⚠️ Advanced Topic"

    Pointers are an advanced feature. If you're new to programming, skip this section for now and come back to it later. Most modern code avoids pointers when possible—use dynamic arrays and classes instead.

- **Description:** A pointer holds the memory address of a variable. They are used for dynamic memory allocation and low-level programming. `^TypeName` declares a pointer to `TypeName`. `Pointer` is a generic pointer type.
- **When to use:** Only when you need direct memory access or working with C libraries. Beginners should avoid!
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