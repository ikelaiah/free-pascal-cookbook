# Numbers


!!! info "Random generator"

    FPC 3.2.2 uses a Mersenne twister generate random numbers.
    
    As for trunk (2023-01-01) FPC uses Xoshiro128
    
    Reason? Xoshiro128 is faster, has a much smaller memory footprint and generates better random numbers.
    
    git: [91cf1774](https://gitlab.com/freepascal.org/fpc/source/-/commit/91cf1774dd59376434dde4581f8eaad74696aa0a)


## How to generate a random number?

Call `Randomize` once, then use [`Random()`](https://www.freepascal.org/docs-html/rtl/system/random.html) function.

```pascal hl_lines="14 17" linenums="1"
program RandomNumberSimple;

{$mode objfpc}{$H+}{$J-}

uses
  Math;

var
  i: integer;

begin

  // Init the random number generator.
  Randomize;

  // Generate a random number betwen 0 to 100
  i := Random(101);
  WriteLn(i);

  // Pause console
  ReadLn;
end.
```

## How to generate a random number (Integer) between 2 numbers?

You can write a function as follows.

```pascal
function RandomBetween(min, max: integer): integer;
begin
  Result:= Random(max - min + 1) + min;
end;
```

Here is the full example.

```pascal hl_lines="3-6 17 19" linenums="1"
program RandomNumberBetween;

function RandomBetween(const min, max: integer): integer;
begin
  Result:= Random(max - min + 1) + min;
end;

var
  randomNumber: integer;
  min, max: integer;

begin
  min := 1;
  max := 10;

  // Initialise the randeom number generator
  Randomize;

  randomNumber := RandomBetween(2, 10);

  WriteLn('Random number between ', min, ' and ', max, ' is: ', randomNumber);

  // Pause console
  ReadLn;
end.
```

## How to generate a random numbers between 2 real numbers?

You can use the function below to get a real number between 2 numbers.

```pascal
function RandomNumberBetween(const min, max: real): real;
begin
  Result := Random * (max - min) + min;
end;
```

Here is the full example.

```pascal hl_lines="3-6 17 20" linenums="1"
program RandomRealNumberBetween;

function RandomNumberBetween(const min, max: real): real;
begin
  Result := Random * (max - min) + min;
end;

var
  randomRealNumber: real;
  min, max: real;

begin
  min := 1.0;
  max := 10.0;

  // Initialise random number generator
  Randomize;

  // Get a random (real) number between min and max
  randomRealNumber := RandomNumberBetween(min, max);

  WriteLn('Random real number between ', min: 0: 6, ' and ', max: 0: 6, ' is: ', randomRealNumber: 0: 6);

  // Pause console
  ReadLn;
end.
```

## How to generate a series of random real numbers?

You can use `TGVector` and `function RandomNumberBetween(const min, max: real): real;` mentioned in the previous section.

1. Create a new `type` to contain a list of real numbers (line 15).
2. Create a new `var` based on the `type` and allocate  (line 22).
3. Initialise the random number generator (line 27).
4. Initialise the new list (line 30).
5. Populate the list using a `for` loop (line 34-38).
6. Free allocated memory for the list (line 54).

```pascal hl_lines="15 22 27 30 34-38 54" linenums="1"
program RandomRealNumberList;

{$mode objfpc}{$H+}{$J-}

uses
  Math, lgVector;

function RandomNumberBetween(const min, max: real): real;
begin
  Result := Random * (max - min) + min;
end;

type
  // Create a list of real (double)
  TRealList = specialize TGVector<real>;

const
  noSample: integer = 250;

var
  i: integer;
  realList: TRealList;

begin
  // Randomize initializes the random number generator by assigning a value
  // to Randseed, which is computed based on the system clock.
  Randomize;

  // Init the real list
  realList := TRealList.Create;
  try

    // Populating the list with random real numbers
    WriteLn('-- Populating the list --------------------------------');
    for i := 0 to noSample do
    begin
      realList.Add(RandomNumberBetween(1, 100));
    end;

    // Displaying the content of the list
    WriteLn('-- Content of list, showing up to 4 decimals ----------');
    for i := 0 to realList.Count - 1 do
    begin
      Write(realList[i]: 0: 4, ' ');
    end;
    WriteLn;

    // Displaying the mean
    WriteLn('-- The mean, up to 4 decimals, is ---------------------');
    WriteLn(Math.Mean(realList.ToArray): 0: 4);

  finally
    // Free allocated resources for list
    realList.Free;
  end;

  ReadLn;
end.
```

Alternatively, you can use `array of real` too.

Here is an example.

```pascal hl_lines="18 26 32" linenums="1"
program RandomRealNumberListv2;

{$mode objfpc}{$H+}{$J-}

uses
  Math;

function RandomNumberBetween(const min, max: real): real;
begin
  Result := Random * (max - min) + min;
end;

const
  noSample: integer = 250;

var
  i: integer;
  realList: array of real;

begin
  // Randomize initializes the random number generator by assigning a value
  // to Randseed, which is computed based on the system clock.
  Randomize;

  // Set size of the dynamic array
  SetLength(realList, noSample);

  // Populating the list with random real numbers
  WriteLn('-- Populating the list --------------------------------');
  for i := low(realList) to high(realList) do
  begin
    realList[i] := RandomNumberBetween(1, 100);
  end;

  // Displaying the content of the list
  WriteLn('-- Content of list, showing up to 4 decimals ----------');
  for i := 0 to high(realList) do
  begin
    Write(realList[i]: 0: 4, ' ');
  end;

  WriteLn;

  // Displaying the mean
  WriteLn('-- The mean, up to 4 decimals, is ---------------------');
  WriteLn(Math.Mean(realList): 0: 4);

  ReadLn;
end.
```