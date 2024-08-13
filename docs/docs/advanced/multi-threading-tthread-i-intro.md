# Multi-threading with `TThread` Part I

## `TThread` Class

Creating a multi-threaded application is easier using the [`TThread`](https://www.freepascal.org/daily/doc/rtl/classes/tthread.html) class. This class lets you add another thread (in addition to the main thread) easily. Usually, you only need to override two methods: the [`constructor Create()`](https://www.freepascal.org/daily/doc/rtl/classes/tthread.create.html) and the [`Execute`](https://www.freepascal.org/daily/doc/rtl/classes/tthread.execute.html) method.

By using `TThread`, you can create and manage multiple threads in your application, making it more efficient and responsive.

### Step-by-Step Guide

1. Declare a descendant of the [`TThread`](https://www.freepascal.org/daily/doc/rtl/classes/tthread.html) object. 

2. Override the **`constructor Create();`**
      - This is where you set up the thread.
      - Initialise variables or properties you need.
      - The constructor has a parameter called `CreateSuspended`:
        - If `CreateSuspended` is set to `True`, the thread won't start immediately.
        - If `CreateSuspended` is set to `False`, the thread will start running right after it's created.
        - If you create the thread with `CreateSuspended := True`, you need to call the `Start` method later to run it.

3. Override **`Execute;`**
      - This is where you write the code that will run in the thread.
      - You can add a loop here to perform repeated actions.

### Important features of TThread

#### `property ProcessorCount: LongWord;`

- Returns the number of cores in the system.

#### `procedure Terminate;`

- The `Terminate` method simply changes the `Terminated` property to `True`.
- It **does not in any way attempt to terminate the thread** in any other way, this just signals the thread that it should stop executing at the earliest possible moment.
  
!!! Important

    When the thread contains a loop (which is common), the loop should end when `Terminated` becomes `True` (by default, it is `False`). During each iteration, check the value of `Terminated`, and if it is `True`, exit the loop promptly after any required cleanup.

#### [`property FreeOnTerminate: Boolean;`](https://www.freepascal.org/daily/doc/rtl/classes/tthread.freeonterminate.html)
  
- If `FreeOnTerminate` is `True`, the thread object is automatically freed when the `Execute` method finishes. This is useful for avoiding memory leaks and simplifying memory management in certain scenarios.
- If `FreeOnTerminate` is `False`, you need to free the thread object manually.
- Use the [OnTerminate](https://www.freepascal.org/docs-html/rtl/classes/tthread.onterminate.html) property to get a notification of when the thread has terminated and will be freed.

!!! Tip - FreeOnTerminate

    When setting `FreeOnTerminate` property to `True`, in general you may not read or write any property of the `TThread` instance from a different thread, because **there is no guarantee that the thread instance still exists in memory**. 
    
    This implies 2 things:

    1. The `OnTerminate` event handler should be set before setting `FreeOnTerminate` to `True`
    2. The properties can still be read and set in the `OnTerminate` event handler, as the thread instance is then still guaranteed to exist.
    
!!! Tip - FreeOnTerminate

    If `FreeOnTerminate` is set to `False`, to stop and delete a running thread from another thread, the following sample code can be used:

    ```pascal
    aThread.Terminate;
    aThread.WaitFor;
    FreeAndNil(aThread); // or aThread.Free;
    ```

    Source: [https://www.freepascal.org/daily/doc/rtl/classes/tthread.freeonterminate.html](https://www.freepascal.org/daily/doc/rtl/classes/tthread.freeonterminate.html)

#### `function WaitFor;`

- `WaitFor` blocks the calling thread until the thread it is called on has finished executing. This is useful for synchronizing the completion of a thread before proceeding with further operations.

!!! Warning

    The potential conflict arises because if a thread is set to `FreeOnTerminate`, it will free itself immediately upon completion. If `WaitFor` is called after the thread has terminated, it will attempt to access a freed (and thus invalid) thread object, leading to undefined behavior or program crashes.

    **Safe Usage Scenarios**

    1. Single Thread with `FreeOnTerminate` and `WaitFor`:

        - You can safely use `WaitFor` with a thread that has `FreeOnTerminate` set to `True` ***if you ensure `WaitFor` is called before the thread completes and frees itself***.
    
    2. For multiple threads, it's safer to avoid combining `FreeOnTerminate` set to `True` with `WaitFor`. Instead, manage the thread lifecycle manually:
   
        - Set `FreeOnTerminate` to `False`.
        - Call `WaitFor` on each thread to ensure it completes.
        - Manually `Free` the thread objects.

!!! Contribution

    Gustavo 'Gus' Carreno 🇵🇹 🇬🇧 — 2024-05-27 at 15:53

    You **should never use** the `Terminate` property of a `TThread` outside the thread itself.

    You **should always use** `WaitFor` *[to wait for the thread to terminate]*!! 

    ```pascal
    begin
      // ...
      MyThread.Terminate;
      MyThread.WaitFor;
      //...
    end;
    ```
    
    Usually, you use `Terminated` extensively in the `Execute` method.

    You kinda have to check it **religiously** inside `Execute`, especially if you have a long running and/or blocking thread.

    But, if I'm not mistaken, the `Terminated` property is privacy level protected. Hence, you **should not use it outside** `Execute`.

    To terminate a thread you call `Terminate`. Then if you need to make sure it's done and has cleaned up, you use `WaitFor`.

#### `procedure Synchronize();`

- Threads **should not directly update visible components** (like UI elements), so you must use  `Synchronize` to safely update UI elements from the thread.
- `Synchronize` ... 
    - pauses the thread, 
    - runs a method (like updating a label) in the main thread, 
    - and then resumes the thread.


### Example: Run a task on a thread with a variable

1. Create a class, for example `TMyThread`, based on `TThread`. Line 14-25.
2. Create a constructor. Line 27-40.
    - call constructor of `TThread`,
    - set free on terminate and
    - Start thread.
3. Override `Execute`. Line 42-52.
    - This procedure contains your task to perform.
4. Create a thread in the main block. Line 67.
5. Wait for the thread to finish and return to the main thread. Line 71.
6. Free the thread manually. Line 74.


```pascal linenums="1" hl_lines="14-25 27-40 42-52 67 71 74"
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

### Example: Run same tasks on multiple threads

!!! Contribution

    2024-02-08 - paweld 🇵🇱 caught a memory leak in the original code and fixed it.

    Thank you!

1. Create a class, for example `TTaskThread`, based on `TThread`. Line 40-47.
2. Override `Execute`. Line 50-57.
    - This procedure contains your task to perform.
3. Create a constructor. Line 60-68.
    - call constructor of `TThread`,
    - set free on terminate and
    - Start thread.
4. Create all threads in the main block. Line 56, 57.
5. Wait for all threads to finish and return to the caller or main thread. Line 88, 89.
6. Free threads manually. Line 92, 93.

```pascal linenums="1" hl_lines="40-47 50-57 60-68 79 80 88 89 92 93"
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