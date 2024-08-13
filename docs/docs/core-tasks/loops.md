# Loops

## What are the loops in Free Pascal?

1. For Loop.

```pascal
for counter := initial_value to final_value do
begin
  // code to run
end;
```

!!! Remarks

    - Free Pascal ==calculates the upper bound once== before setting the counter variable.
    - You can't change the value of a loop variable inside the loop.
    - The loop variable's value is unclear after the loop ends or if the loop doesn't run. If the loop stops early due to an exception, break, or goto statement, the variable keeps its last value.
    - If you're using nested procedures, the loop variable must be a local variable. Using a loop variable outside the nested procedure will cause a compiler error, but using a global variable is allowed.

    Adapted from [https://www.freepascal.org/docs-html/ref/refsu58.html#x168-19200013.2.4]()

1. For-In Loop.

```pascal
for item in collection do
begin
  // code to run
end;
```

3. While Loop.

```pascal
while condition do
begin
  // code to run
end;
```

4. Repeat Until Loop.

```pascal
repeat
  // code to run
until condition;
```

5. Nested Loops.

```pascal
// Basically, a loop inside another loop
// A simple example (among many others)

for counter_a := initial_value_a to final_value_a do
  for counter_b := initial_value_b to final_value_b do
  begin
    // code to run
  end;

```

### Examples

The snippet below demonstrates different types of loops in Free Pascal.

```pascal linenums="1"
program Loops;

  {$mode objfpc}{$H+}{$J-}

var
  intArray: array [0..2] of integer = (10, 20, 30);
  strArray: array of string = ('Apple', 'Banana', 'Cirtus');
  i, j: integer; // vars for iteration
  item: string; // var for iterating a collection of string
  c: char = char(0);
begin

  // 1a. For Loop -------------------------------
  for i := 0 to 2 do
  begin
    WriteLn('For Loop: Value of i is ', intArray[i]);
  end;

  WriteLn('--------------------');

  // 1b. For Loop using low & high --------------
  for i := low(intArray) to high(intArray) do
  begin
    WriteLn('For Loop with low & high: ', intArray[i]);
  end;

  WriteLn('--------------------');

  //2a. For-In Loop -----------------------------
  for i in intArray do
  begin
    WriteLn('For-In Loop - integer: ', i);
  end;

  //2b. For-In Loop -----------------------------
  for item in strArray do
  begin
    WriteLn('For-In Loop - string: ', item);
  end;

  WriteLn('--------------------');

  // 3. While Loop ------------------------------
  j := 0;
  while j <= 5 do
  begin
    WriteLn('While Loop from 0 until 5: ', j);
    Inc(j);
  end;

  WriteLn('--------------------');

  // 4. Repeat Until Loop -----------------------
  repeat
    Write('Repeat Until Loop: What is the next letter after ''a''? ');
    ReadLn(c);
  until c = 'b';
  WriteLn('Yes, b is the correct answer');

  WriteLn('--------------------');

  // 5. An example of a Nested Loops
  for item in strArray do
    for i := low(intArray) to high(intArray) do
    begin
      Writeln('Nested Loops: For ', item, ', You can buy in pack of ', intArray[i]);
    end;

  WriteLn('--------------------');

  // Pause console
  ReadLn;
end.
```

If you run the program, the output would be as follows (with `b` to answer the question in the Repeat Until loop).

```bash linenums="1"
For Loop: Value of i is 10
For Loop: Value of i is 20
For Loop: Value of i is 30
--------------------
For Loop with low & high: 10
For Loop with low & high: 20
For Loop with low & high: 30
--------------------
For-In Loop - integer: 10
For-In Loop - integer: 20
For-In Loop - integer: 30
For-In Loop - string: Apple
For-In Loop - string: Banana
For-In Loop - string: Cirtus
--------------------
While Loop from 0 until 5: 0
While Loop from 0 until 5: 1
While Loop from 0 until 5: 2
While Loop from 0 until 5: 3
While Loop from 0 until 5: 4
While Loop from 0 until 5: 5
--------------------
Repeat Until Loop: What is the next letter after 'a'? b
Yes, b is the correct answer
--------------------
Nested Loops: For Apple, You can buy in pack of 10
Nested Loops: For Apple, You can buy in pack of 20
Nested Loops: For Apple, You can buy in pack of 30
Nested Loops: For Banana, You can buy in pack of 10
Nested Loops: For Banana, You can buy in pack of 20
Nested Loops: For Banana, You can buy in pack of 30
Nested Loops: For Cirtus, You can buy in pack of 10
Nested Loops: For Cirtus, You can buy in pack of 20
Nested Loops: For Cirtus, You can buy in pack of 30
--------------------

```

## What are best use cases for each loop?

### For Loop

Best for:

- Iterating a block of code over a fixed number of times.
- Keeping track of loop counter.

For examples:

- Reading and processing all elements in a fixed-size array.
- Copying N files from one location to another.

### For-In Loop 

Best for:

- Looping through collections, arrays, or enumerable types without manual indexing.
- Iterating tasks without a loop counter.

Examples:

- Printing all names in a list of people.
- Checking if an object exists in an collection.


### While Loop

Best for:

- Repeating a block of code as long as a condition remains true, without knowing the exact number of repetitions in advance.

Examples:

- Reading user input until they enter a specific value.
- Reading command line options until no more options are found.

### Repeat Until Loop

Best for:

- Like While Loop, but ensuring execution of a block of code **at least once**, even if the initial condition is false.

Examples:

- A login system that repeatedly requests credentials until authentication is successful.
- Reading a CSV file while ensuring that specific columns are present before processing its contents.


### Nested Loops

Best for:

- Working with N-dimensional arrays or collections.
- Generating patterns based on multiple arrays.

Examples:

- Populating a 3x3 array of integer.
- Finding possible combination of words between N array of strings.
