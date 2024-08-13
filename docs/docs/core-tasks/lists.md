# Building and Processing Lists

## What is a list?

Think of it as a higher-level construct than a static or dynamic array.

Higher-level constructs:

- Abstract away lower-level details, allowing developers to work with more generalised and easier-to-use interfaces.
- Reduce the amount of manual code a developer needs to write.

## Why do we want to use lists? 

Well, conveniences.

- You may need to change the length of your lists often at run-time.
- You may need your lists for more than just storing and retreival (sort and search).
- Etc.

Here is a brief comparison between `array of string` and `TStringList` ([https://wiki.freepascal.org/TStringList-TStrings_Tutorial](https://wiki.freepascal.org/TStringList-TStrings_Tutorial)).

Operation            | array of string                                                                                                                  | TStringList                     
-------------------- | -------------------------------------------------------------------------------------------------------------------------------- | --------------------------------
Variable declaration | `stringList: array of string;`                                                                                                   | `stringList: TStringList;`        
Initialization       | implicit constructor                                                                                                             | `stringList := TStringList.Create;`
Set size             | `SetLength(StringList, X);`                                                                                                      | `stringList.Size := n;`           
Get size             | `n := Length(stringList);`                                                                                                       | `n := stringList.Count;`          
Add item             | `SetLength(stringList, Length(stringList) + 1); stringList[Length(stringList) - 1] := x;`                                        | `stringList.Add(x);`              
Delete item          | `for i := index to Length(stringList) - 2 do stringList[i] := stringList[i + 1]; SetLength(stringList, Length(stringList) - 1);` | `stringList.Delete(index);`       
Remove all items     | `SetLength(stringList, 0);`                                                                                                      | `stringList.Clear;`               
Finalisation         | `implicit destructor`                                                                                                            | `stringList.Free;`                


## How can I make a list of string and sort it?

Use [`TStringList`](https://lazarus-ccr.sourceforge.io/docs/rtl/classes/tstringlist.html). See the recipe below.

1. Create a var of  `TStringList`. Line 9.
2. Create the `TStringList`. Line 16.
3. Append items.
4. `Sort` it. Line 33.
5. Finally, `Free` the `TStringList`. Line 63.

```pascal linenums="1" hl_lines="9 16 33 63"
program StringList;

{$mode objfpc}{$H+}{$J-}

uses
  Classes;

var
  myStringList: TStringList;
  stringIndex: integer;
  i: integer;

begin

  // Allocate a memory for this list
  myStringList := TStringList.Create;
  try
    // Add items into the list
    myStringList.Add('cc');
    myStringList.Add('aa');
    myStringList.Add('bb');

    // Insert at index-n, 0-indexed
    myStringList.Insert(0, 'zero');

    // Remove item at index-n
    myStringList.Delete(0);

    // Append an array of string
    myStringList.AddStrings(['yy', 'zz - last one']);

    // Sort list
    myStringList.Sort;

    // Find an exact match string, case insensitive, un-sorted
    // A sorted TStringlist has myStringList.Sorted:=True;
    stringIndex := myStringList.IndexOf('yy');
    if stringIndex >= 0 then
    begin
      WriteLn('Found yy at index: ', stringIndex);
    end;

    // Iteration for partial string mathching
    for i := 0 to myStringList.Count - 1 do
    begin
      if Pos('last one', myStringList[i]) > 0 then
      begin
        // String found at index "i"
        WriteLn('Found ''last one'' at index: ', i);
        Break; // Exit the loop if a match is found
      end;
    end;

    // Iterate through the list
    for i := 0 to myStringList.Count - 1 do
      Writeln(myStringList[i]);

    // Clear the list, size of the list will be 0
    myStringList.Clear;

  finally
    // Free the list
    myStringList.Free;
  end;

  // Pause Console
  Readln;

end.
```

## Is there a `TIntegerList`?

No. Make your own type.

## Using `Generics.Collections` unit

### How do I make a list of integer and sort it?

Here is an example.

1. Use [`Generics.Collections.TList`](https://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate1/EN/pdf/genericcollections.pdf) to create `TIntegerList`.
2. Declare type `TIntegerList = specialize TList<integer>;`. Line 11.
3. Create a new variable using the new list type - line 14.
4. In the main `begin..end` block, allocate memory for the list. Line 19.
5. Populate the list. Line 22-24.
6. Call `Sort`. Line 30
7. Lastly, free it. Line 44.


```pascal linenums="1" hl_lines="11 14 19 22-24 30 44"
program SimpleIntegerList;

{$mode objfpc}{$H+}{$J-}

uses
  Math,
  Generics.Defaults,
  Generics.Collections;

type
  TIntegerList = specialize TList<integer>;

var
  myIntList: TIntegerList;
  i: integer;

begin
  // Create a new generic list
  myIntList := TIntegerList.Create;
  try
    // Add some elements to the list, use Add or AddRange (append)
    myIntList.Add(0);
    myIntList.Add(1);
    myIntList.AddRange([9, 8, 7, 6, 5]);

    // Access the n-th element, 0-indexed
    WriteLn('The 3rd item is: ', myIntList[2]);

    // Sorting it ascending
    myIntList.Sort;

    // Iterate through the list
    for i := 0 to myIntList.Count - 1 do
      Writeln(myIntList[i]);

    // Get the mean
    WriteLn('The mean is: ', Math.Mean(myIntList.ToArray): 0: 2);

    // Empty the list
    myIntList.Clear;

  finally
    // Free the memory used by the list
    myIntList.Free;
  end;

  // Pause console
  ReadLn;
end.
```

### How to create a custom comparer for `TList<T>` list?

Here is an example of sorting `TList<TStudent>` by name and by age.

- Set a custom comparer for `TList<TStudent>`. See line 36.
- Define comparison functions. See line 39-49.
- Lastly, call the Sort using the comparison function. See line 75 and 83.

```pascal hl_lines="36 39-49 75 83" linenums="1"
program TListCustomComparison;

{$mode objfpc}{$H+}{$J-}{$modeSwitch advancedRecords}

uses
  Generics.Defaults,
  Generics.Collections,
  SysUtils,
  Math;

type
  TStudent = record
  var
    Name: string;
    age: byte;
    location: string;
  public
    constructor Create(newName: string; newAge: byte; newLocation: string);
    function ToString: string;
  end;

  constructor TStudent.Create(newName: string; newAge: byte; newLocation: string);
  begin
    self.Name := newName;
    self.age := newAge;
    self.location := newLocation;
  end;

  function TStudent.ToString: string;
  begin
    Result := 'Row data: ' + self.Name + ' ' + IntToStr(self.age) + ' ' + self.location;
  end;

type
  TStudentList = specialize TList<TStudent>;
  TStudentListComparer = specialize TComparer<TStudent>;


  // Custom comparison function for sorting by name - ascending
  function CompareName(constref LeftItem, RightItem: TStudent): integer;
  begin
    Result := CompareStr(LeftItem.Name, RightItem.Name);
  end;

  // Custom comparison function for sorting by age - ascending
  function CompareAge(constref LeftItem, RightItem: TStudent): integer;
  begin
    Result := CompareValue(LeftItem.age, RightItem.age);
  end;

var
  studentList: TStudentList;
  i: integer;

begin
  // Create a new generic list
  studentList := TStudentList.Create;
  try
    // Add some elements to the list by using Add or AddRange
    studentList.Add(TStudent.Create('Asher Mo', 10, 'Sydney'));
    studentList.AddRange(
      [TStudent.Create('Kezia Mo', 10, 'Sydney'),
      TStudent.Create('Irene Mo', 11, 'Sydney'),
      TStudent.Create('Jonah Mo', 12, 'Sydney'),
      TStudent.Create('Bob Yell', 13, 'Melbourne'),
      TStudent.Create('Luke Earthwalker', 9, 'Canberra')]
      );

    // Printing list on console
    WriteLn('-- Original list ------');
    for i := 0 to studentList.Count - 1 do
      Writeln(studentList[i].ToString);

    // Sort by TStudent.name
    studentList.Sort(TStudentListComparer.construct(@CompareName));

    // Iterate through the list
    WriteLn('-- Sorted by name ------');
    for i := 0 to studentList.Count - 1 do
      Writeln(studentList[i].ToString);

    // Sort by TStudent.age
    studentList.Sort(TStudentListComparer.construct(@CompareAge));

    // Iterate through the list
    WriteLn('-- Sorted by age ------');
    for i := 0 to studentList.Count - 1 do
      Writeln(studentList[i].ToString);

  finally
    // Free the memory used by the list
    studentList.Free;

  end;

  // Pausing console
  ReadLn;
end.
```

## Using `fgl` unit

### How do I build a list of integer using `fgl` and sort it?

See the snippet below.

1. In the uses section, add `fgl`. Line 6.
2. Create a list type using `specialize TFPGList<integer>s`. Line 10.
3. Create a integer comparer function for sorting. Line 13-16.
4. Create a new var using the new type. Line 23.
5. Add items using `Add()`. Line 26.
6. Sort list using `Sort(@TCompareFunc)`. Line 35.
7. Release the memory using `Free()`. Line Line 43.

```pascal linenums="1" hl_lines="6 10 13-16 23 35 43"
program FGLIntegerList;

{$mode objfpc}{$H+}{$J-}

uses
  fgl,
  Math;

type
  TIntegerList = specialize TFPGList<integer>;

  // Comparer function for sorting list
  function CompareInt(const left, right: integer): integer;
  begin
    Result := CompareValue(left, right);
  end;

var
  myIntList: TIntegerList;
  i: integer;

begin
  myIntList := TIntegerList.Create;
  try
    // Adding integers to the list
    myIntList.Add(444);
    myIntList.Add(222);
    myIntList.Add(333);

    // Printing list
    Writeln('-- Original list ----------');
    for i := 0 to myIntList.Count - 1 do WriteLn(myIntList[i]);

    // Sorting
    myIntList.Sort(@CompareInt);

    // Printing list
    Writeln('-- Sorted list ------------');
    for i := 0 to myIntList.Count - 1 do WriteLn(myIntList[i]);

  finally
    // Freeing the list when done
    myIntList.Free;
  end;

  // Pause console
  ReadLn;
end.
```

### How do you append two `TFPGList<Integer>` lists?

Use `AddList()`. Here is an example.

```pascal hl_lines="26" linenums="1"
program AppendFPGList;

{$mode objfpc}{$H+}{$J-}

uses
  fgl,
  Math;

type
  TIntegerList = specialize TFPGList<integer>;

var
  myIntList1, myIntList2: TIntegerList;
  i: integer;

begin
  myIntList1 := TIntegerList.Create;
  myIntList2 := TIntegerList.Create;

  // Adding integers to the lists
  myIntList1.Add(1);
  myIntList1.Add(2);
  myIntList2.Add(3);
  myIntList2.Add(4);

  myIntList1.AddList(myIntList2);

  // Printing the first list
  WriteLn('The content of myIntList1 is now:');
  for i := 0 to myIntList1.Count - 1 do
  begin
    WriteLn(myIntList1[i]);
  end;

  // Freeing the lists when done
  myIntList2.Free;
  myIntList1.Free;

  ReadLn;
end. 
```

## When building a list, which unit is preferable to use: `fgl` or `Generics.Collections`?

Consider this answer from PascalDragon, March 22, 2020, 12:55 pm.

> Just to clear these up as well:
> 
> - `fgl` is a unit of generic types distributed with FPC that is smaller than `Generics.Collections` and has some restrictions and the performance might be worse; however it can be more easily be used where size of the binary is a concern
> - `Generics.Collections` is the name of the Delphi-compatible generic collection types (list, dictionary, etc.) which is rather powerful and performant; this is part of FPC 3.2.0 and newer ...
> 
> Source: https://forum.lazarus.freepascal.org/index.php?topic=48988.0

## External - Using`LGenerics` unit

Before trying out snippet in this section, make sure you have installed `LGenerics` via the `Online Package Manager (OPM)` in Lazarus IDE.

### Creating a list of integer using `LGenerics`

1. In `uses` add `lgVector`.
2. Create a new type based on `TGVector<T>`.
3. Declare a `var` to use this new list type.
4. Create a new instance of the new list for use. The constructor takes an array. See line 18.
5. Lastly, don't forget to `Free` your memory as shown in line 33.

```pascal hl_lines="7 10 13 18 21-23 33" linenums="1"
program LGIntegerList;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  lgVector;

type
  TIntVector = specialize TGVector<integer>;

var
  myInteger: TIntVector;
  i: integer;

begin
  // Creating a new instance
  myInteger := TIntVector.Create([11, 33, 22, 55, 44, 66]);
  try
    // Adding numbers
    myInteger.Add(1);
    myInteger.Add(10);
    myInteger.Add(100);

    // Printing list
    for i := 0 to myInteger.Count - 1 do
    begin
      WriteLn(myInteger[i]);
    end;

  finally
    // Releasing resource
    myInteger.Free;
  end;

  ReadLn;
end.
```

### Can I sort a `TGVector<integer>` list?

Easy. Follow the step below if your integer list is `TGVector<integer>`.

1. Add `lgUtils` to the `uses` section.
2. Add `TIntOrdHelper = specialize TGOrdVectorHelper<integer>;` in the `type` section.

That's it. 

To sort, use the `TIntOrdHelper.Sort()` or `TIntOrdHelper.RadixSort()` methods.

See the following example.

```pascal hl_lines="8 12 35 45" linenums="1"
program LGIntegerListSort;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  lgVector,
  lgUtils;

type
  TIntVector = specialize TGVector<integer>;
  TIntOrdHelper = specialize TGOrdVectorHelper<integer>;

var
  myInteger: TIntVector;
  i: integer;

begin
  // Creating a new instance of TGVector<integer>
  myInteger := TIntVector.Create([11, 33, 22, 55, 44, 66]);
  try
    // Adding numbers
    myInteger.Add(1);
    myInteger.Add(10);
    myInteger.Add(100);

    // Printing list
    WriteLn('--original');
    for i := 0 to myInteger.Count - 1 do
    begin
      WriteLn(myInteger[i]);
    end;

    // Sorting descending using .Sort()
    TIntOrdHelper.Sort(myInteger, TSortOrder.soDesc);

    // Printing sorted list
    WriteLn('--sorted desc');
    for i := 0 to myInteger.Count - 1 do
    begin
      WriteLn(myInteger[i]);
    end;

    // Sorting ascending using .RadixSort()
    TIntOrdHelper.RadixSort(myInteger, TSortOrder.soAsc);

    // Printing sorted list
    WriteLn('--sorted asc');
    for i := 0 to myInteger.Count - 1 do
    begin
      WriteLn(myInteger[i]);
    end;

  finally
    myInteger.Free;
  end;

  ReadLn;
end.
```