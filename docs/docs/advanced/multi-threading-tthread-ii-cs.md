# Multi-threading with `TThread` - Part II. Critical Section

## Why We Need Critical Sections

Imagine you and your friends are working on a group project, and you all need to update a shared document. If everyone tries to edit the document at the same time, changes might get lost or mixed up. To prevent this, you take turns updating the document. This is similar to what happens in a program when multiple parts (threads) need to access and modify shared data.

## The Problem with Concurrent Access

In a program, you might have multiple threads running at the same time, each trying to read or write to the same piece of data. If this access isn’t controlled, it can lead to:

- **Data Corruption**: Two threads might try to update the data simultaneously, leading to incorrect results.
- **Inconsistent State**: The shared data might end up in an unpredictable state, causing bugs that are hard to find and fix.

## What is a Critical Section?

A critical section is a part of your code that you want to protect so that only one thread can execute it at a time. It’s like setting up a rule where only one person can edit the document at a time to ensure that changes are made safely and correctly.

## How it Works in Free Pascal

In Free Pascal, you use critical sections to control access to shared data.

Critical Section as a Lock: Think of a critical section as a lock that you put around the shared document. When someone wants to edit the document, they have to take the lock, do their changes, and then give the lock back. This ensures that only one person can edit at a time.

**How to Use Critical Sections**

- `EnterCriticalSection`: Before running the code you want to protect, call `EnterCriticalSection`. This ensures that the current thread is the only one running the protected code. This call may make the thread wait until it's safe to proceed.

- `LeaveCriticalSection`: After the protected code has finished, call `LeaveCriticalSection`. This allows other threads to run the protected code.

!!! tip 
    To minimise waiting time for threads, keep the protected code block as small as possible.

**Setup and Cleanup**

- `InitCriticalSection`: Initializss the critical section. Must be called before using `EnterCriticalSection` or `LeaveCriticalSection`.
- `DoneCriticalSection`: Cleans up the critical section. **Must be called** when you're done with the critical section.



Here is an example of using Critical Section to increment a counter variable.

```pascal linenums="1" hl_lines="47 101-104 127-130 66-70 80-82"
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

### Explanation of the Code

1. Declare and Initialise:

      - `criticalSection` is declared as a variable of type `TRTLCriticalSection`. Line 47.
      - `InitCriticalSection(criticalSection)` initialises the critical section (lock). Line 104.

2. Enter and Leave Critical Section:

      - `EnterCriticalSection(criticalSection)` is like taking the lock. Only one thread can do this at a time. Line 70.
      - `LeaveCriticalSection(criticalSection)` releases the lock, allowing another thread to take it. Line 82.

3. Protecting Shared Data:

      - The `IncrementCounter` procedure increments the counter safely because the code inside the critical section can only be executed by one thread at a time. Line 146.

4. Cleanup:

      - `DoneCriticalSection(criticalSection)` cleans up the critical section when it's no longer needed. Line 130.