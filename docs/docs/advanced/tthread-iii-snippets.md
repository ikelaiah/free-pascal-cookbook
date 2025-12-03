# Multi-threading with `TThread` Part III - Snippets

Here are collections of snippets solving various tasks using multi-threading.

## Run a task on a thread with a variable

```pascal linenums="1"
program EX1SingleThread;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes { you can add units after this };

type
  // The TThread class encapsulates the native thread support of the OS.
  // To create a thread, (1) declare a child of the TThread object, ...
  TMyThread = class(TThread)
    // (with a data to work with)
  private
    aString: string;
  protected
    // (2) override the Execute method, and ...
    procedure Execute; override;
  public
    // (3) lastly, you may include a constructor to setup variables
    // for executing this thread.
    constructor Create(isSuspended: boolean; message: string);
  end;

  constructor TMyThread.Create(isSuspended: boolean; message: string);
  begin

    // Call parent's constructor
    // If user pass True, thread won't start automatically
    inherited Create(isSuspended);

    // Assign a data to work with.
    self.aString:=message;

    // Won't free the thread when finished.
    // The thread will be freed manually on the main block.
    FreeOnTerminate:=False;
  end;

  procedure TMyThread.Execute;
  begin
    // Execute thread, and DO SOMETHING in this thread.

    // Example: if the thread has a data to work with,
    //          use it to achieve a goal.
    WriteLn('Thread ID ', ThreadID, ' is printing ', self.aString);

    // Example: simulate a long running process.
    Sleep(1000);
  end;

var
  mythread:TMyThread;

// Main block --------------------------------------------------
begin

// Create a thread, suspended
myThread:=TMyThread.Create(True, 'Hello, World!');

// Debug line
WriteLn('We are in the main thread');

// Start the thread
myThread.Start;

// Wait until the thread is done before going back to
// the main thread.
myThread.WaitFor;

// Free threads manually.
myThread.Free;

// Debug line.
WriteLn('We are in the main thread again');

// Pause console.
WriteLn('Press enter key to quit');
ReadLn;

end.
```

## Run same tasks on multiple threads

```pascal linenums="1"
program EX2MultiThread;

{
  # EX2MultiThread

  A simple demo of multi-threading.

  ## Example of an output

  ---------------------
  Started TThread demo
  ---------------------
  Starting a task from the main thread
  Started a task on thread ID 22848
  Started a task on thread ID 24428
  Completed the task from the main thread
  Completed task on thread ID: 22848
  Completed task on thread ID: 24428
  ---------------------
  Finished TThread demo
  Press Enter to quit
  ---------------------
}


{$mode objfpc}{$H+}{$J-}

// 2024-02-08 - paweld 🇵🇱 fixed a memory leak issue on the original code.

uses
  {$ifdef unix}
  cmem, cthreads,
  {$endif}
  Classes,
  SysUtils;

type
  // Create a class based on TThread
  // TTaskThread
  TTaskThread = class(TThread)
  protected
    // Override the Execute procedure of TThread
    procedure Execute; override;
  public
    // Thread constructor with free on terminate
    constructor Create;
  end;

  // The Execute procedure, simulating a task
  procedure TTaskThread.Execute;
  begin
    WriteLn('Started a task on thread ID ', ThreadID);

    Sleep(2000); // Simulating a long-running task.

    WriteLn('Completed task on thread ID: ', ThreadID);
  end;

  // Constructor of TTaskThread
  constructor TTaskThread.Create;
  begin
    // Create as suspended.
    inherited Create(True);
    // Set Free on Terminate to false, so it won't free itself when completed.
    FreeOnTerminate := False;
    // Run thread.
    Start;
  end;

var
  task1, task2: TThread;

begin
  WriteLn('---------------------');
  WriteLn('Started TThread demo');
  WriteLn('---------------------');

  // Create all threads
  task1 := TTaskThread.Create;
  task2 := TTaskThread.Create;

  // Start a task on the main thread
  Writeln('Starting a task from the main thread');
  Sleep(2000); // simulate a task
  Writeln('Completed the task from the main thread');

  // Wait for threads to finish before going back to the main thread.
  task1.WaitFor;
  task2.WaitFor;

  // Free the threads manually
  task1.Free;
  task2.Free;

  WriteLn('---------------------');
  WriteLn('Finished TThread demo');
  WriteLn('Press Enter to quit');
  WriteLn('---------------------');
  ReadLn;
end.
```

## Sum numbers in an array

Important features of this example.

1. Use of `TIntegerDynArray`, which is a dynamic array of integers.
2. Thread Class `TMyThread` inherits from `TThread`. This class encapsulates the logic for summing a segment of the array within each thread.
3. The constructor `TMyThread.Create` initialises the thread with the input array, start index, and end index. This setup ensures each thread works on a specific portion of the array.
4. The `Execute` method in `TMyThread` performs the actual summing of numbers within the assigned segment of the array.
5. Main program
      - Calculates the segment size for each thread using `Math.Ceil((Length(inputArray) + MAX_THREADS - 1) / MAX_THREADS)`, ensuring that each thread gets an equal or almost equal portion of the array to process.
      - Creates and starts each thread, passing in the relevant segment of the array.
      - After all threads complete their work, collect the partial sums from each thread and calculate the total sum.
6. Manual memory cleanup by setting `FreeOnTerminate := False` and manually freeing the threads after collecting their results.
7. Lastly, the program displays information about each thread's segment and its partial sum.

```pascal linenums="1"
program EX3MultiThread;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  Types,
  Generics.Collections,
  Math { you can add units after this };

  // --- Custom thread type --------------------------------------
type
  // The TThread class encapsulates the native thread support of the OS.
  // To create a thread, (1) declare a child of the TThread object, ...
  TMyThread = class(TThread)
    // (with a data to work with)
  private
    // Variables to store the input array for this thread to sum, along with
    // start index and end index
    anArray: TIntegerDynArray;
    startIdx: integer;
    endIdx: integer;
    // The sum of array in the thread
    partialSum: integer;
  protected
    // (2) override the Execute method, and ...
    procedure Execute; override;
  public
    // (3) lastly, you may include a constructor to setup variables
    // for executing this thread.
    constructor Create(const isSuspended: boolean;
                       var   inputArray: TIntegerDynArray;
                       const startIndex: integer;
                       const endIndex: integer);
  end;

  constructor TMyThread.Create(const isSuspended: boolean;
                               var   inputArray: TIntegerDynArray;
                               const startIndex: integer;
                               const endIndex: integer);
  begin
    // Call parent's constructor
    // If user pass True, thread won't start automatically
    inherited Create(isSuspended);

    // Assign a data to work with.
    self.anArray := inputArray;
    self.startIdx := startIndex;
    self.endIdx := endIndex;

    // DO NOT Free thread when finished here.
    // The main thread will ...
    //   1. collect the results from n threads,
    //   2. free n threads from the main thread.
    FreeOnTerminate := False;
  end;

  procedure TMyThread.Execute;
  var
    index: integer;
  begin
    // Execute thread, and DO SOMETHING in this thread.

    // Initialise partialSum to 0 to start with
    self.partialSum := 0;

    // partialSum the numbers in the assigned array using for..do loop.
    for index := self.startIdx to self.endIdx do
      self.partialSum := self.partialSum + self.anArray[index];

    // Display user feedback from this thread
    WriteLn('Thread ', ThreadID, ' summed up ', self.partialSum);
  end;

// const and var for the main block ----------------------------
const
  // Specify max number of threads to use.
  MAX_THREADS = 4;
  // The length of an input array may come from a file.
  INPUT_ARRAY_LENGTH = 10000;

var
  // Input array containg numbers to sum.
  inputArray: TIntegerDynArray;
  // Setting an array for the threads.
  myThreads: array of TMyThread;
  // Size of a segment for each thread
  segmentSize: integer;
  // total sum -- will collect values from each thread
  totalSum: integer;
  // Indexes
  index, startIndex, endIndex: integer;


  // Main block ------------------------------------------------
begin

  // Populate input array. This may come from a file.
  SetLength(inputArray, INPUT_ARRAY_LENGTH);
  for index := 0 to INPUT_ARRAY_LENGTH - 1 do
    inputArray[index] := index + 1;


  // Calculate segment size for each thread
  segmentSize := Math.Ceil((Length(inputArray) + MAX_THREADS - 1) / MAX_THREADS);

  // Create and start the threads.
  SetLength(myThreads, MAX_THREADS);
  for index := 0 to MAX_THREADS - 1 do
  begin
    // Start index for a thread is i * segmentSize.
    startIndex := index * SegmentSize;
    // Ensure that each thread processes the correct portion of the array
    // without going out of bounds on last iteration.
    endIndex := Min((index + 1) * SegmentSize - 1, Length(inputArray) - 1);

    // Show user info.
    WriteLn('startIndex: ', startIndex, ' ', ' endIndex:', endIndex);

    // Create a thread.
    myThreads[index] := TMyThread.Create(False, inputArray, StartIndex, EndIndex);
    // Start this new thread.
    myThreads[index].Start;
  end;

  // Wait until a thread is done, sum up and free it.
  totalSum := 0;
  for index := 0 to MAX_THREADS - 1 do
  begin
    // Wait until thread index n finishes
    myThreads[index].WaitFor;
    // Get the partial sum from thread index n
    totalSum := totalSum + myThreads[index].partialSum;
    // Lastly, free thread index n
    myThreads[index].Free;
  end;

  // Display results
  WriteLn('Total sum of array is: ', totalSum);

  WriteLn('Press enter key to quit');
  ReadLn;
end.
```


## Increment a counter multi-threading

This snippet features:

- Use of `TRTLCriticalSection` to ensure only one thread can increment a counter variable

```pascal
program CriticalSectionIncrementCounter;

{

  An example adapted from \lazarus\examples\multithreading\multithread_critical
  for CLI.
  This is a simple example using 4 threads to increase a counter (shared
  variable).

  To enable critical sections, set isCriticalSectionEnabled to True.
  To disable critical sections, set isCriticalSectionEnabled to False.

  With critical sections you will always get 4,000,000.
  Without you will see different results on each run.

  Important: In most Unix like systems, the cthread unit must be added
             to the uses section of the .lpr file. Further, cmem is likely to
             be significently faster so add it as well. Due to how the units
             work a sensible order is cmem, cthreads and then perhaps cwstrings.
             But note that heaptrc does not work with cmem so comment it out
             while testing/debugging.
}

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils;

type
  TCustomThread = class(TTHread)
  private
    finishedState: boolean;
  public
    procedure Execute; override;
    property isFinished: boolean read finishedState write finishedState;
  end;

const
  // Use Critical Section or not?
  isCriticalSectionEnabled: boolean = True;

var
  criticalSection: TRTLCriticalSection;
  mainCounter: integer;

  procedure TCustomThread.Execute;
  var
    i: integer;
    currentCounter: longint;
  begin
    // Set the finished state to false.
    finishedState := False;

    // Increment the mainCounter.
    // Because the other threads are doing the same, it will frequently happen
    // that 2 (or more) threads read the same number, increment it by one and
    // write the result back, overwriting the result of the other threads.
    for i := 1 to 1000000 do
    begin

      if isCriticalSectionEnabled then
        // 2. Begins the lock.
        //    When this call returns, the calling thread is the only thread
        //    running the code between the EnterCriticalSection call and the
        //    following LeaveCriticalsection call.
        EnterCriticalSection(criticalSection);
      try
        // Read the current mainCounter
        currentCounter := mainCounter;
        // Increment mainCounter by one
        Inc(currentCounter);
        // Write the result back the mainCounter variable
        mainCounter := currentCounter;
      finally
        if isCriticalSectionEnabled then
          // 3. Releases the lock.
          //    Signals that the protected code can be executed by other threads.
          LeaveCriticalSection(criticalSection);
      end;
    end;

    // Once the task for this thread is done, set the finished state to True.
    finishedState := True;
  end;

  {
   This is the routine that increment a counter
  }
  procedure IncrementCounter;
  var
    index: integer;
    threadList: array[1..4] of TCustomThread;
    isAllThreadsFinished:boolean;
  begin
    mainCounter := 0;

    // 1. Initialises a critical section.
    //    This call must be made before either EnterCrititicalSection or
    //    LeaveCriticalSection is used.
    InitCriticalSection(criticalSection);

    // Start the threadList
    for index := Low(threadList) to High(threadList) do
      threadList[index] := TCustomThread.Create(False);

    WriteLn('All threads created ...');

    // Wait till all threadList finished
    repeat
      isAllThreadsFinished := True;
      for index := Low(threadList) to High(threadList) do
        if not threadList[index].isFinished then isAllThreadsFinished := False;
    until isAllThreadsFinished;

    WriteLn('All threads completed ...');

    // Free the threadList
    for index := Low(threadList) to High(threadList) do
      threadList[index].Free;

    WriteLn('All threads are freed ...');

    // 4. Frees the resources associated with a critical section.
    //    After this call neither EnterCrititicalSection nor LeaveCriticalSection
    //    may be used.
    DoneCriticalSection(criticalSection);

    // Show the mainCounter
    WriteLn('Printing the value of shared variable ...');
    WriteLn('Counter = ' + IntToStr(mainCounter));
  end;

// Main block ------------------------------------------------------------------
begin
  // Print status of Critical Section
  if isCriticalSectionEnabled then
    WriteLn('Critical Section: Enabled')
  else
    WriteLn('Critical Section: Disabled');

  // Callt he routine to increment a counter using multi-threading
  IncrementCounter;

  // Pause console
  WriteLn('Press Enter key to exit.');
  ReadLn;
end.
```


## Assigns student IDs to student names

This program assigns an ID to each student name from a text file and sorts them by student ID.

This snippet features:

- `TFileStream` and `TStreamReader` for reading lines from a text file
- Use of four threads to complete the task
- Use of rounding-up division to split the workload between threads
- Use of `TRTLCriticalSection` to ensure only one thread can write to the output list


### The `common.pas` of assigning IDs to names

This file holds the common type and variable declarations.

There are two important common variables here:

1. `startStudentID: int64 = 200000;` - This variable specifies the starting index for student IDs. All threads will read from this variable and increment it by one for other threads to read from. Therefore, reading and incrementing this variable MUST be done within a critical section to avoid race conditions.
2. `finalStudentList: TStudentList;` - A list of TStudent records containing names and student IDs. All threads will write their output here, so writing to this variable MUST also be done within a critical section.
  

```pascal linenums="1"
unit Common;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, Math;


type
  // A record to hold student information.
  TStudent = record
    Name: string;
    id: integer;
  end;

type
  // A list to hold student records, along with a comparer for
  // sorting the list afterwards.
  TStudentList = specialize TList<TStudent>;
  TStudentListComparer = specialize TComparer<TStudent>;

type
  // A type of string list.
  TStrList = specialize TList<string>;

const
  // Number of maximum threads to use.
  maxThreads: int64 = 4;

var
  // This variable specifies the lowest index for student ID.
  // All threads will be reading from this variable and increase by one
  // for other threads to read from.
  // Hence, reading and increment of this variable MUST be done
  // from within a critical section to avoid race.
  startStudentID: int64 = 200000;

  // A list of TStudent records. All threads will write student names and
  // student IDs into this variable.
  // Hence, writing to this variable MUST be done from within
  // a critical section too.
  finalStudentList: TStudentList;

// Custom comparison function for sorting by name - ascending
function CompareID(constref LeftItem, RightItem: TStudent): integer;

implementation

// Custom comparison function for sorting by student id - ascending
function CompareID(constref LeftItem, RightItem: TStudent): integer;
begin
  Result := CompareValue(LeftItem.id, RightItem.id);
end;

end.
```

### The `customthread.pas` of assigning IDs to names

This file defines the implementation of a custom thread based on `TThread`.

There are important things to note here:

1. The implementation includes a `destructor` to clean up the list used by the threads for the task (lines 30, 63-69).
2. The thread receives an array but only processes a portion of it based on the rounding-up division algorithm defined in the main block.
3. `FreeOnTerminate := False` as the main thread is responsible for managing the freeing of all threads (line 47).
4. The `Execute` method updates shared variables within a critical section (lines 79-91).


```pascal linenums="1"  hl_lines="48 30 64-70 80-92"
unit CustomThread;

{$mode ObjFPC}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Common;

  // Create a thread class deriving from TThread.
type
  // The TThread class encapsulates the native thread support of the OS.
  // To create a thread, (1) declare a child of the TThread object, ...
  TCustomThread = class(TThread)
    // (with a data to work with)
  private
    // A TStrList to store the input array for this thread, along with
    // a variable to store an instance of critical section.
    list: TStrList;
    cs:TRTLCriticalSection;
  protected
    // (2) override the Execute method, and ...
    procedure Execute; override;
  public
    // (3) include a constructor to setup variables for executing this thread.
    constructor Create(const criticalSection: TRTLCriticalSection;
                       const listToProcess: TStrList;
                       const startIndex, finishIndex: int64);
    // (4) lastly, include  destructor to free the TStrList of this thread.
    destructor Destroy; override;
  end;

implementation

// Create the Custom Thread with an input list to process.
constructor TCustomThread.Create(const criticalSection: TRTLCriticalSection;
                                 const listToProcess: TStrList;
                                 const startIndex, finishIndex: int64);
var
  index: int64;
begin
  // Call parent's constructor
  // If user pass True, thread won't start automatically
  inherited Create(True);

  // Not free threads on terminate.
  // Threads will be freed from the main thread.
  FreeOnTerminate := False;

  // Assign critical section
  self.cs := criticalSection;

  // Populate the internal list for the Execute procedure
  self.list := TStrList.Create;
  for index := startIndex to finishIndex do
  begin
    self.list.Add(listToProcess[index]);
  end;

  // User feedback
  WriteLn('Thread created with id: ', ThreadID);
end;

destructor TCustomThread.Destroy;
begin
  // Free the TStrList.
  self.list.Free;
  // Call parents' Destroy.
  inherited Destroy;
end;

// Enter and leave Critical Section here.
procedure TCustomThread.Execute;
var
  index: int64;
  student: TStudent;
begin
  for index := 0 to self.list.Count - 1 do
  begin
    EnterCriticalSection(cs); // --------------------------------- enter cs
    try
      // Add student - ID pair as TStudent, then add into TStudentList
      //   1. Get the name from the list with allocated index
      student.Name := list[index];
      //   2. Get the starting student ID from
      student.id := startStudentID;
      //   3. Add TStudent into TStudentList (the main block does the init)
      finalStudentList.Add(student);
      // After a student - ID pair is added, increment the current student ID by 1
      startStudentID := startStudentID + 1
    finally
      LeaveCriticalSection(cs); // ------------------------------- leave cs
    end;
  end;
end;

end.
```

### The main program of assigning IDs to names

Key features of the main snippet:

1. The text file is read into `strList`.
2. Threads are created and assigned specific subarrays to process.
3. Critical sections ensure safe access to shared variables.
4. After the threads finish, the results are sorted and printed.

```pascal linenums="1"
program AssignStudentIDs;

{
 This program assigns student ID to each name from a text file by
 using N number of threads.

 Pre-requisite

    - TThread for managing the threads.
    - TRTLCriticalSection for ensuring only one thread can modify a shared
      variable at one time.
    - Input text file containing a list of names. For example;

    Alyssa Morgan
    Declan Hayes
    Nora Patel
    Miles Thompson
    Sienna Larson
    Kellan Rivera
    Camille Chang
    Jensen Park
    Amara Singh
    Holden Myers
    Elise Howard
    Luca Griffin
    Reagan Patel
    Kian Gallagher
    Mara Nguyen
    ...
    ...

 Algorithm

    - Read the text file into an array.
      - All threads will read from the same array, but at differing start and
        finish indexes. This depends on the number of max threads.
    - Assign workloads to each thread.
      - Specify the start and finish indexes to each thread.
      - Will use the rounding up division method to ensure near-equal division
        of workload for each thread.
    - Wait for the threads to finish and then free them.
    - Sort the final student list
    - Print results on screen.

 Sample Output

   $ ./AssignStudentIDs.exe student-names.txt
   No of students         : 200
   Max threads            : 4
   subArray size round up : 51
   ---------------------------------
   Thread created 25040
   Thread created 26324
   Thread created 11972
   Thread created 26028
   Starting threads ...
   Waiting for threads to finish ...
   All threads are done ...
   Printing results ...
   Alyssa Morgan, 200000
   Declan Hayes, 200001
   Nora Patel, 200002
   Miles Thompson, 200003
   Sienna Larson, 200004
   Kellan Rivera, 200005
   Camille Chang, 200006
   Jensen Park, 200007
   Amara Singh, 200008
   Holden Myers, 200009
   Elise Howard, 200010
   Luca Griffin, 200011
   Reagan Patel, 200012
   ...
   ...

}

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem
  , cthreads
  {$ENDIF}
  Classes,
  SysUtils,
  Math,
  streamex,
  Common,
  CustomThread;

  // All the variables and procedures to get the job done.
var
  // Critical Section preventing threads accessing a variable at the same time.
  customCriticalSection: TRTLCriticalSection;

  // TStreams for reading files
  fileStream: TFileStream;
  streamReader: TStreamReader;

  // A temporary list to hold student names from a text file
  strList: TStrList;

  // An array of threads
  myThreads: array of TThread;

  // Variables for calculating subarray size for each thread
  subArraySize, index: int64;

  // Main block ////////////////////////////////////////////////////////////////
begin
  try
    // 1. Read input text file and populate input array from a text file
    if not FileExists(ParamStr(1)) then
    begin
      WriteLn(Format('Input file: %s does not exist. Did you provide the correct path?', [ParamStr(1)]));
      Exit;
    end;

    strList := TStrList.Create;
    finalStudentList := TStudentList.Create;
    try
      fileStream := TFileStream.Create(ParamStr(1), fmOpenRead);
      try
        streamReader := TStreamReader.Create(fileStream, 65536, False);
        try
          while not streamReader.EOF do
          begin
            // Add each line into a list
            strList.Add(streamReader.ReadLine);
          end;
        finally
          streamReader.Free;
        end;
      finally
        fileStream.Free
      end;

      // 2. Init Critical Section as we have threads writing to a shared variable
      InitCriticalSection(customCriticalSection);

      // 3a. set the number of threads in array of TThread
      SetLength(myThreads, maxThreads);
      try
        {
          3b. Now we add threads & assign workloads, using rounding up division --
              Ceil((totalElements + N - 1) div N).
              - When we divide totalElements by N, we get the quotient of the
                division. However, if totalElements is not evenly divisible by N,
                there might be a remainder.
              - In the context of splitting an array into subarrays, we want
                each subarray to have approximately the same number of elements.
                Therefore, we want to ensure that any remaining elements after
                dividing totalElements by N are included in the last subarray to
                avoid losing data.
              - The expression Ceil((totalElements + N - 1) div N); effectively
                rounds up the division by adding N - 1 to totalElements before
                performing the division. **This ensures that any remainder is
                accounted for in the last subarray**.
        }
        subArraySize := Math.Ceil((strList.Count + maxThreads - 1) / maxThreads);

        // Show user feedback
        WriteLn('No of students         : ', IntToStr(strList.Count));
        WriteLn('Max threads            : ', IntToStr(maxThreads));
        WriteLn('subArray size round up : ', IntToStr(subArraySize));
        WriteLn('---------------------------------');

        {
         3c. Assign workload to each thread by using the following info;
             - source list
             - start index and
             - finish index for a thread
        }
        for index := 1 to maxThreads do
        begin
          myThreads[index - 1] := TCustomThread.Create(customCriticalSection,
                                                       strList,
                                                       ((index - 1) * subArraySize),
                                                       Math.Min(index * subArraySize - 1, strList.Count - 1));
        end;

        // 4. Start all threads
        WriteLn('Starting threads ...');
        for index := 0 to High(myThreads) do
          myThreads[index].Start;

        // 5. Wait for both threads to finish and free
        WriteLn('Waiting for threads to finish ...');
        for index := 0 to High(myThreads) do
          begin
            myThreads[index].WaitFor;
            myThreads[index].Free;
          end;
        WriteLn('All threads are done ...');

        // 6. Sort by student ID
        finalStudentList.Sort(TStudentListComparer.construct(@CompareID));

        // 7. Show results
        WriteLn('Printing results ...');
        for index := 0 to finalStudentList.Count - 1 do
          WriteLn(finalStudentList[index].Name, ', ', finalStudentList[index].id);

        // 8. Show user feedback
        WriteLn('---------------------------------');
        WriteLn(Format('Output list contains %d items', [finalStudentList.Count]));
        WriteLn('---------------------------------');
      finally
        // 7. Free Critical Section
        DoneCriticalSection(customCriticalSection);
      end;
    finally
      finalStudentList.Free;
      strList.Free;
    end;

  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;
end.
```

## A Large Text File Parser

This example demonstrates how to parse a text file using multi-threading. The code reads the text file, divides it into chunks, and assigns each chunk to a different thread. Each thread then prints the number of bytes it processed and the first line of its chunk.

```pascal linenums="1"
program LargeTextFileParser;

{
 Description

 This program is a simple example of parsing of a text file using N threads.
 The code reads and divides the text file into chunks and assigns these chunks
 to different threads, ensuring that no chunk splits a paragraph or sentence.

 Workflow

 1. File Reading      - Read the file in 12MB chunks.
 2. Data Integrity    - Ensure chunks do not split paragraphs or sentences by
                        adjusting the file pointer based on the last newline
                        character.
 3. Thread Management - Use up to N threads to process chunks in parallel.
 4. Display Output    - Print the size of each chunk and the first line of each chunk.
}

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  Math;

const
  MAX_THREADS = 8; // Max threads to use
  CHUNK_SIZE = 12 * 1024 * 1024; // Size of chunk for each thread to process (12MB)

type
  // The TThread class encapsulates the native thread support of the OS.
  TFileChunkProcessor = class(TThread)
  private
    FData: array of char; // Chunk of data to work with
    FDataSize: integer;   // Length of data to the nearest new line
  protected
    procedure Execute; override;
  public
    constructor Create(const AData: array of char; ADataSize: integer);
  end;

  constructor TFileChunkProcessor.Create(const AData: array of char; ADataSize: integer);
  begin
    // Call parent's constructor and don't start thread automatically
    inherited Create(True);

    // Assign a AData size to work with
    FDataSize := ADataSize;

    // Allocate memory for AData and copy it
    // Since Char in Free Pascal can be 1 or 2 bytes (depending on whether
    // it's an ANSI or Unicode character), using SizeOf(Char) ensures
    // the correct number of bytes are moved
    SetLength(FData, FDataSize);
    Move(AData[0], FData[0], FDataSize * SizeOf(char));

    // Do not free thread automatically when finished.
    FreeOnTerminate := False;
  end;

  procedure TFileChunkProcessor.Execute;
  var
    line: string;
    index: integer;
  begin
    // Example processing: print the chunk size and the first line
    line := '';
    for index := 0 to FDataSize - 1 do
    begin
      if FData[index] = #10 then
      begin
        Writeln('Processed chunk of size: ', FDataSize, ' bytes');
        Writeln('First line: ', line);
        Break;
      end
      else if FData[index] <> #13 then
      begin
        line := line + FData[index];
      end;
    end;
  end;

  {
   This routine reads a text file in chunks and processes each chunk using
   separate threads. It ensures each thread processes a chunk up to the
   nearest newline, without breaking a paragraph or sentence.
  }
  procedure ReadFileInChunks(const AFileName: string);
  var
    fStream: TFileStream;
    buffer: array of char;
    bufferSize: integer;
    index, lastNewLine: integer;
    threadList: array of TFileChunkProcessor;
    activeThreads: integer;
  begin
    fStream := TFileStream.Create(AFileName, fmOpenRead);
    try

      // Initialise variables for buffer, threads and threads counter.
      SetLength(buffer, CHUNK_SIZE);
      SetLength(threadList, MAX_THREADS);
      activeThreads := 0;

      // Read the file until the file pointer reaches the end of the file
      while fStream.Position < fStream.Size do
      begin
        // Determine the buffer size to read and ensuring that the buffer size
        // for reading does not exceed the size of the remaining data in the file.
        bufferSize := Min(CHUNK_SIZE, (fStream.Size - fStream.Position) div SizeOf(char));

        // Read data into buffer
        fStream.Read(buffer[0], bufferSize);

         // Find the index of the last newline character in the buffer
        lastNewLine := -1;
        for index := bufferSize - 1 downto 0 do
        begin
          if buffer[index] = #10 then
          begin
            lastNewLine := index;
            Break;
          end;
        end;

        if lastNewLine = -1 then
          lastNewLine := bufferSize - 1;

        // Create a thread to process the chunk and its size (index of \n + 1)
        threadList[activeThreads] := TFileChunkProcessor.Create(buffer, lastNewLine + 1);
        threadList[activeThreads].Start;

        {
          Next, adjust file position to the character after the last newline.

          ---
          Explanation
          ---

          Let's say we have a buffer size of 1000 characters, and the last
          newline character is found at index 950.
          The buffer contains 50 characters after the last newline.

          bufferSize  = 1000
          lastNewLine = 950
          bufferSize - lastNewLine - 1 = 1000 - 950 - 1 = 49
          Assuming SizeOf(Char) = 1 (for simplicity),

          ```pascal
          fStream.Position := fStream.Position - 49 * SizeOf(Char);
          ```

          This means the file position is moved back by 49 bytes, so the next
          read operation will start at the 951st character in the buffer,
          which is the character immediately following the last newline.
          This ensures that no line is split between two chunks and maintains
          the integrity of the data being processed.
        }
        fStream.Position := fStream.Position - (bufferSize - lastNewLine - 1) * SizeOf(char);

        // Increment active thread counter
        Inc(activeThreads);

        // If max threads are active, wait for them to finish
        if activeThreads = MAX_THREADS then
        begin
          for index := 0 to MAX_THREADS - 1 do
          begin
            threadList[index].WaitFor;
          end;
          activeThreads := 0;
        end;
      end; // -- End of the `while fStream.Position < fStream.Size do` loop.

      // Wait for any remaining threads to complete
      for index := 0 to activeThreads - 1 do
      begin
        threadList[index].WaitFor;
      end;

    finally
      fStream.Free; // Clean up the file stream
    end;
  end;

  // MAIN block ----------------------------------------------------------------
begin

  // Check if the input file exists
  if not FileExists(ParamStr(1)) then
  begin
    WriteLn('File not found. Does ', ParamStr(1), ' exist?');
    Exit;
  end;

  // Parse the text file using multi-threading
  try
    ReadFileInChunks(ParamStr(1));
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
```