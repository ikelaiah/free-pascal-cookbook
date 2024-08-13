# Common Data Types in Free Pascal

Here is a list of common data types in Free Pascal, along with a simple example.

See the official docs for more info; [Types](https://www.freepascal.org/docs-html/ref/refch3.html#x24-230003).

## Summary Table

| **Type** | **Data Type** | **Range / Size** |
| --- | --- | --- |
| Integer | `Byte` | 0 .. 255 |
| Integer | `ShortInt` | \-128 .. 127 |
| Integer | `SmallInt` | \-32,768 .. 32,767 |
| Integer | `Integer` | \-2,147,483,648 .. 2,147,483,647 |
| Integer | `LongInt` | Same as `Integer` |
| Integer | `Int64` | \-9,223,372,036,854,775,808 .. 9,223,372,036,854,775,807 |
| Integer | `Word` | 0 .. 65,535 |
| Integer | `Cardinal` | 0 .. 4,294,967,295 |
| Integer | `QWord` | 0 .. 18,446,744,073,709,551,615 |
| Boolean | `Boolean` | `True` or `False` |
| Character | `Char` | ASCII characters |
| String | `String` | Default length 255 characters |
| Floating-Point | `Single` | ±1.5 x 10^−45 .. ±3.4 x 10^38 |
| Floating-Point | `Real` | Similar to `Single` |
| Floating-Point | `Double` | ±5.0 x 10^−324 .. ±1.7 x 10^308 |
| Floating-Point | `Extended` | More precise than `Double` (varies by implementation) |
| Enumerated | `Enum` | User-defined values |
| Subrange | `Subrange` | Defined range |
| Record | `Record` | User-defined complex type |
| Advanced Record | `Adv Record` | Records with methods |
| Array | `Static Array` | Fixed size |
| Array | `Dynamic Array` | Variable size |
| Pointer | `Pointer` | Memory address of a type |

## Integer Types

### Byte
    
- **Range:** 0 .. 255
- **Example:**
        
```pascal linenums="1"
var
  b: Byte; 
begin   
  b := 100;   
  WriteLn(b); 
end.
```
        
### ShortInt
    
- **Range:** -128 .. 127
- **Example:**
        
```pascal linenums="1"
var   
  s: ShortInt; 
begin   
  s := -50;   
  WriteLn(s); 
end.
```
        
### **SmallInt**
    
- **Range:** -32,768 .. 32,767
- **Example:**
        
```pascal linenums="1"
var
  sm: SmallInt;
begin
  sm := 32000;
  WriteLn(sm);
end.
```
        
### Integer
    
- **Range:** -2,147,483,648 .. 2,147,483,647
- **Example:**
        
```pascal linenums="1"
var
  i: Integer;
begin
  i := 1234567890;
  WriteLn(i);
end.
```

### LongInt
    
- **Range:** Same as `Integer`
- **Example:**
        
```pascal linenums="1"
var
  li: LongInt;
begin
  li := -2147483648;
  WriteLn(li);
end.
```

### Int64
    
- **Range:** -9,223,372,036,854,775,808 .. 9,223,372,036,854,775,807
- **Example:**
        
```pascal linenums="1"
var
  i64: Int64;
begin
  i64 := 9223372036854775807;
  WriteLn(i64);
end.
```

### Word
    
- **Range:** 0 .. 65,535
- **Example:**
        
```pascal linenums="1"
var
  w: Word;
begin
  w := 65535;
  WriteLn(w);
end.
```

### Cardinal
    
- **Range:** 0 .. 4,294,967,295
- **Example:**
        
```pascal linenums="1"
var
  c: Cardinal;
begin
  c := 4294967295;
  WriteLn(c);
end.
```

### QWord
    
- **Range:** 0 .. 18,446,744,073,709,551,615
- **Example:**
        
```pascal linenums="1"
var
  qw: QWord;
begin
  qw := 18446744073709551615;
  WriteLn(qw);
end.
```


## Boolean

- **Values:** `True` or `False`
- **Example:**
        
```pascal linenums="1"
var
  qw: QWord;
begin
  qw := 18446744073709551615;
  WriteLn(qw);
end.
```


## Char (Character)

- **Range:** 'A' .. 'Z', 'a' .. 'z', and other ASCII characters
- **Example:**
        
```pascal linenums="1"
var
  c: Char;
begin
  c := 'A';
  WriteLn(c);
end.
```


## String

- **Length:** Default length is 255 characters
- **Example:**
        
```pascal linenums="1"
var
  str: String;
begin
  str := 'Hello, Pascal!';
  WriteLn(str);
end.
```


## **Floating-Point Types**

### Single
    
- **Range:** Approximately ±1.5 x 10^−45 .. ±3.4 x 10^38
- **Example:**
        
```pascal linenums="1"
var
  s: Single;
begin
  s := 3.14;
  WriteLn(s);
end.
```

### Real
    
- **Range:** Varies by implementation, usually similar to `Single`
- **Example:**
        
```pascal linenums="1"
var
  r: Real;
begin
  r := 2.71828;
  WriteLn(r);
end.
```

### **Double**
    
- **Range:** Approximately ±5.0 x 10^−324 .. ±1.7 x 10^308
- **Example:**


```pascal linenums="1"
var
  d: Double;
begin
  d := 1.7976931348623157E+308;
  WriteLn(d);
end.
```


### **Extended**
    
- **Range:** Varies by implementation, generally more precise than `Double`
- **Example:**
        
```pascal linenums="1"
var
  e: Extended;
begin
  e := 1.0e+308;
  WriteLn(e);
end.
```        

## Enumerated Types

- **Example:**
    
```pascal linenums="1"
type
  TColor = (Red, Green, Blue);
var
  color: TColor;
begin
  color := Green;
  WriteLn(color);
end.
```

## Subrange Types

- **Example:**
    
```pascal linenums="1"
type
  TSmallRange = 1..100;
var
  r: TSmallRange;
begin
  r := 50;
  WriteLn(r);
end.
```    

## Record Types

- **Example:**
    
```pascal linenums="1"
type
  TPerson = record
    Name: String;
    Age: Integer;
  end;
var
  person: TPerson;
begin
  person.Name := 'Alice';
  person.Age := 30;
  WriteLn(person.Name, ' is ', person.Age, ' years old.');
end.
```    

## Advanced Records (with methods)

- **Example:**
    
```pascal linenums="1"
type
  TAdvancedPerson = record
    Name: String;
    Age: Integer;
    procedure DisplayInfo;
  end;
procedure TAdvancedPerson.DisplayInfo;
begin
  WriteLn(Name, ' is ', Age, ' years old.');
end;
var
  person: TAdvancedPerson;
begin
  person.Name := 'Bob';
  person.Age := 40;
  person.DisplayInfo;
end.    
```

## Arrays

### Static Array
    
- **Example:**
        
```pascal linenums="1"
var   
  arr: array[1..5] of Integer; 
begin   
  arr[1] := 10;
  WriteLn(arr[1]);
end.
```

### Dynamic Array
    
- **Example:**
        
```pascal linenums="1"
var
  arr: array of Integer;
  i: Integer;
begin
  SetLength(arr, 5);

  for i := 0 .. High(arr) do
    arr[i] := i * 10;

  for i := 0 .. High(arr) do
    WriteLn(arr[i]);
end.
```        

## **Pointers**

- **Example:**
    
```pascal linenums="1"
var
  p: ^Integer;
  x: Integer; 
begin   
  New(p);
  p^ := 10;
  x := p^;
  WriteLn(x);
  Dispose(p); 
end. 
```