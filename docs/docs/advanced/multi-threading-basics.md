# Multi-threading - Do you need it?

## What is it?

!!! info
    The information in this section is adapted from the following URL; [Multithreaded Application Tutorial](https://wiki.freepascal.org/Multithreaded_Application_Tutorial), excluding the tutorial.

A multi-threaded application can run multiple tasks simultaneously by creating separate threads. The Main Thread manages user interactions, while other threads handle background tasks.

This setup improves application responsiveness during intensive operations. Multi-threading is beneficial for running tasks in the background, maintaining user interface responsiveness, and managing multiple client requests in server applications.

### Do you need multi-threading?

Multi-threading is good for:

- Managing blocking handles like network tasks.
- Using multiple processors at once to process a large dataset.
- Using multiple processors to process different parts of an image simultaneously, improving efficiency and speed.
- Etc.

Before using multi-threading to speed up tasks using many processors, make sure of the following items. 

1. Your program is already using all resources well on one CPU core. 
2. Check if your program is optimized at the highest level (level 3) for the best performance, as higher optimization can make your program faster.

### Multi-threaded apps are complex and harder to debug

For simpler tasks, one thread may be enough. 

Instead of many threads, you can split long tasks into smaller parts or use `Application.ProcessMessages` to handle user actions during long tasks.

!!! info
    What is `Application.ProcessMessages`?
    
    `Application.ProcessMessages` signals that the app can execute events from its event queue. Let's say that you have 2 buttons on a form with to `onclick` procedures assigned. The first procedure is a lengthly process (eg. `repeat..until true`). The second button has only `ShowMessage('haha')`.

    Now, without `Appllication.ProcessMessages` inserted in the first procedure in the repeat statement, if you press the first button then you will not be able to press the seccond button (or anything else) until the repeat statement finishes. So the user interface is frozen.

    With the `application.processmessages` inserted as follows

    ```pascal
    repeat
      Application.ProcessMessages;
      ...
    until true;
    ```

    If you press the first button and then the second button the `ShowMessage` will happen! So, it is a way to fake a multithread app :-))

    Source: [Lazarus: The effect of Application.ProcessMessages](https://stackoverflow.com/a/24789033/1179312)


### Units needed for a multi-threaded application

#### Windows

You don´t need any special unit for this to work with Windows. 

#### Linux macOS FreeBSD

With Linux, macOS and FreeBSD, you need the `cthreads` unit and it must be the first used unit of the project (the program source, usually the `.lpr` file)! 

In cases where you need  units like `cmem`, `cthreads`, and `cwstrings`; 

1. Place them first in the `uses` section, 
2. Due to how these units work, a sensible order is `cmem`, `cthreads` and `cwstrings`.

So, your FPC/Lazarus application code should begin as the following snippet.


```pascal linenums="1"
program AMultiThreadedProgram;

{$mode objfpc}{$H+}{$J-}
uses
{$ifdef unix}
  cmem, // the c memory manager is faster for multi-threading
        // on some systems
  cthreads,
{$endif}
  { you can add units here };

  // ... the rest of your code
```