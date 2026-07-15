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

Before using multiple threads to speed up CPU-bound work:

1. Measure the program first and identify the actual bottleneck.
2. Check that the work can be divided into independent parts.
3. Consider the cost of creating threads, synchronising shared state, and
   combining the results.

### Multi-threaded apps are complex and harder to debug

For simpler tasks, one thread may be enough. In a GUI application, small jobs
can sometimes be split into short timer or queued-event steps. Blocking I/O and
CPU-intensive work should normally run in a worker `TThread`, with UI updates
synchronised back to the main thread.

`Application.ProcessMessages` only processes the current thread's event queue;
it does not create parallel execution. Calling it from inside a long event
handler can also let other handlers run while the application state is only
partly updated. It is therefore not a general substitute for a worker thread.


### Units needed for a multi-threaded application

#### Windows

Windows does not require an additional thread-manager unit.

#### Linux, macOS, and FreeBSD

On Unix-like systems, include `cthreads` first in the program's `uses` section,
or second when `cmem` is deliberately listed first. If the application needs
wide-string locale support, include `cwstring` near the beginning too.

`cmem` is optional: use it only when you intentionally want the C memory
manager, not merely because the application is multi-threaded. A minimal
cross-platform program can begin as follows.


```pascal linenums="1"
program AMultiThreadedProgram;

{$mode objfpc}{$H+}{$J-}
uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Classes;

begin
  WriteLn('Thread support initialized.');
end.
```

See the Free Pascal documentation for the [`cthreads` startup
requirements](https://www.freepascal.org/daily/doc/user/userse50.html) and the
[`cwstring` unit](https://www.freepascal.org/docs-html/rtl/cwstring/index.html).
