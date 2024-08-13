# ezthreads

> A simple and safe way to work with threads.
>
> -Highball

Code repo: [https://github.com/mr-highball/ezthreads](https://github.com/mr-highball/ezthreads).

## Features

- argument capturing by using the `.AddArg()` method
- reference counted threads ensuring memory is cleaned up
- `Await()` support method (similar to c#) can wait for threads, groups, all threads, pools to complete
- ez thread pool for fixed worker size and easy to use fluent methods
- works with nested, callbacks, object methods (or all at the same time)
- events for start, stop, error, success, etc... for flexibility

## Setup

1. Make sure you have FPC and/or Lazarus installed.
2. Get the library. You have two choices here.

    - Download the source code and unzip on your local drive, or 
    - `git clone` the repo on your local drive.

3. Include the library's `src/` in your project path.
4. *(Optional but highly recommended)* Thank the author of ezthreads.

## Examples

!!! Info
    Prior running ezthreads snippets on this page, make sure two important things.
    
    1. You've downloaded the source code from [https://github.com/mr-highball/ezthreads](https://github.com/mr-highball/ezthreads)
    2. Your Free Pascal/Lazarus project includes `src/*.pas` files of the ezthreads.


### Tasks on Threads

```pascal linenums="1"
program ezthreadsSimple;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads, // Include units for threading support on UNIX systems
  {$ENDIF}
  Classes,
  SysUtils,
  ezthreads;

  // Method to be executed by Thread A
  procedure MethodA(const AThread: IEZThread);
  begin
    // Simulate some important work in Thread A
    WriteLn('TestThread::ThreadA starting');
    Sleep(1000); // Simulate work with a 1-seconds delay
    WriteLn('TestThread::ThreadA finished');
  end;

  // Method to be executed by Thread B
  procedure MethodB(const AThread: IEZThread);
  begin
    // Simulate some important work in Thread B
    WriteLn('TestThread::ThreadB starting');
    Sleep(1000); // Simulate work with a 1-second delay
    WriteLn('TestThread::ThreadB finished');
  end;

var
  LThreadA, LThreadB: IEZThread; // Declare thread variables

  // Main block
begin
  // Initialize both threads
  LThreadA := NewEZThread;
  LThreadB := NewEZThread;

  // Setup and start Thread A
  LThreadA
    .Setup(@MethodA) // Assign MethodA to Thread A
    .Start;          // Start Thread A

  // Setup Thread B to wait for Thread A to finish before starting its task
  LThreadB
    .Setup(@MethodB) // Assign MethodB to Thread B
    .Start;          // Start Thread B

  // Wait for all threads to complete
  Await;

  // Wait for user input to keep the console window open
  WriteLn('Press Enter key to quit');
  ReadLn;
end.
```

### Thread Dependency

```pascal linenums="1"
program ezthreadsDependency;

{$mode objfpc}{$H+}{$J-}
{$modeswitch nestedprocvars}

uses
  {$IFDEF UNIX}
  cmem, cthreads, // Include units for threading support on UNIX systems
  {$ENDIF}
  Classes,
  SysUtils,
  ezthreads;

{
  This example demonstrates a scenario where one thread (B) depends
  on another thread (A) to finish before it can proceed. The main thread
  waits for both threads (A) and (B) to complete using the await mechanism.
}
  procedure TestThreadDependency;
  var
    LThreadA, LThreadB: IEZThread; // Declare thread variables

  // Method to be executed by Thread A
    procedure MethodA(const AThread: IEZThread);
    begin
      // Simulate some important work in Thread A
      WriteLn('TestThreadDependency::ThreadA starting');
      Sleep(1000); // Simulate work with a 1-second delay
      WriteLn('TestThreadDependency::ThreadA finished');
    end;

    // Method to be executed by Thread B
    procedure MethodB(const AThread: IEZThread);
    var
      LID: string;
    begin
      LID := AThread['id']; // Retrieve the ID of Thread A

      // Indicate that Thread B is starting
      WriteLn('TestThreadDependency::ThreadB starting');

      // Wait until Thread A has completed its execution
      Await(LID);

      // Indicate that Thread B has finished
      WriteLn('TestThreadDependency::ThreadB finished');
    end;

  begin
    // Initialize both threads
    LThreadA := NewEZThread;
    LThreadB := NewEZThread;

    // Setup and start Thread A
    LThreadA
      .Setup(@MethodA) // Assign MethodA to Thread A
      .Start;          // Start Thread A

    // Setup Thread B to wait for Thread A to finish before starting its task
    LThreadB
      .AddArg('id', LThreadA.Settings.Await.GroupID) // Pass the GroupID of Thread A to Thread B
      .Setup(@MethodB) // Assign MethodB to Thread B
      .Start;          // Start Thread B

    // Wait for all threads to complete
    Await;
  end;

  // Main block
begin
  // Execute the thread dependency test
  TestThreadDependency;
  // Wait for user input to keep the console window open
  ReadLn;
end.
```

### Thread Pool

```pascal linenums="1"
program ezthreadsPool;

{$mode objfpc}{$H+}{$J-}
{$modeswitch nestedprocvars}

uses
  {$IFDEF UNIX}
  cmem, cthreads, // Include units for threading support on UNIX systems
  {$ENDIF}
  Classes,
  SysUtils,
  ezthreads, ezthreads.pool; // Include units for thread pool support

// A procedure to test two tasks running in parallel using a thread pool
procedure TestTwoTasks;
var
  LPool: IEZThreadPool;
  LJobOneFinished, LJobTwoFinished: boolean; // Flags to indicate if the jobs are finished

  // A simple parallel job (JobOne)
  procedure JobOne(const AThread: IEZThread);
  begin
    // Simulate some work with a sleep of 100 ms
    Sleep(100);
    // Mark the job as finished
    LJobOneFinished := True;
  end;

  // Another simple parallel job (JobTwo)
  procedure JobTwo(const AThread: IEZThread);
  begin
    // Simulate some work with a sleep of 100 ms
    Sleep(100);
    // Mark the job as finished
    LJobTwoFinished := True;
  end;

begin
  // Initialise flags for checking if both jobs finished
  LJobOneFinished := False;
  LJobTwoFinished := False;

  // Initialise a pool with two worker threads
  LPool := NewEZThreadPool(2);

  // Queue two jobs to the pool
  LPool
    .Queue(@JobOne, nil, nil) // Queue JobOne to the pool
    .Queue(@JobTwo, nil, nil) // Queue JobTwo to the pool
    .Start; // Start the pool, which begins executing the jobs

  // Wait until both jobs finish
  Await(LPool);

  // Write the status to the console
  WriteLn(Format('TestTwoTasks::[success]:%s',
                 [BoolToStr(LJobOneFinished and LJobTwoFinished, True)]));
end;

// Main block
begin
  // Run the demo procedure that tests two tasks in parallel
  TestTwoTasks;

  // Wait for user input to keep the console window open
  WriteLn('Press Enter key to quit');
  ReadLn;
end.

```