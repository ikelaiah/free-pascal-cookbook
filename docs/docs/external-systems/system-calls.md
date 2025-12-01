# Running External Programs and System Calls

Sometimes you need to run other programs from your Free Pascal code. For example, you might want to run a command, capture its output, or interact with system tools.

## Simple System Calls

To run a command and wait for it to finish:

```pascal linenums="1"
program SimpleSystemCall;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils;

begin
  WriteLn('Running a system command...');

  { Run the command and wait for it to finish }
  ExecuteProcess('notepad', []);

  WriteLn('Command finished');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Running Commands with Arguments

Many commands need arguments (parameters). Here's how to pass them:

```pascal linenums="1"
program CommandWithArgs;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils;

begin
  WriteLn('Creating a text file...');

  { Run cmd to create a file (Windows) }
  ExecuteProcess('cmd.exe', ['/C', 'echo Hello > test.txt']);

  WriteLn('File created: test.txt');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Capturing Command Output

If you want to see what the command outputs, use a different approach:

```pascal linenums="1"
program CaptureOutput;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  Process;

var
  MyProcess: TProcess;
  StringList: TStringList;
  i: integer;

begin
  { Create a process to run a command }
  MyProcess := TProcess.Create(nil);
  StringList := TStringList.Create;

  try
    { Set the command to run }
    MyProcess.Executable := 'cmd.exe';
    MyProcess.Parameters.Add('/C');
    MyProcess.Parameters.Add('dir');  { List files (Windows) }

    { Capture the output }
    MyProcess.Options := MyProcess.Options + [poWaitOnExit, poUsePipes];
    MyProcess.Execute;

    { Read the output }
    StringList.LoadFromStream(MyProcess.Output);

    WriteLn('=== Output ===');
    for i := 0 to StringList.Count - 1 do
      WriteLn(StringList[i]);

  finally
    MyProcess.Free;
    StringList.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Cross-Platform Commands

Different operating systems have different commands. Use conditional compilation to handle this:

```pascal linenums="1"
program CrossPlatform;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  Process;

var
  MyProcess: TProcess;
  Command: string;
  Output: TStringList;

begin
  MyProcess := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    { Choose command based on OS }
    {$IFDEF WINDOWS}
    Command := 'cmd.exe';
    MyProcess.Parameters.Add('/C');
    MyProcess.Parameters.Add('dir');
    {$ENDIF}

    {$IFDEF UNIX}
    Command := '/bin/ls';
    {$ENDIF}

    MyProcess.Executable := Command;
    MyProcess.Options := MyProcess.Options + [poWaitOnExit, poUsePipes];
    MyProcess.Execute;

    Output.LoadFromStream(MyProcess.Output);

    WriteLn('=== Files in current directory ===');
    for var Line in Output do
      WriteLn(Line);

  finally
    MyProcess.Free;
    Output.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Getting Exit Codes

Programs return exit codes to tell you if they succeeded or failed:

```pascal linenums="1"
program CheckExitCode;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  Process;

var
  MyProcess: TProcess;
  ExitCode: integer;

begin
  MyProcess := TProcess.Create(nil);

  try
    MyProcess.Executable := 'cmd.exe';
    MyProcess.Parameters.Add('/C');
    MyProcess.Parameters.Add('dir /S /B nonexistent.txt');  { This will fail }

    MyProcess.Options := MyProcess.Options + [poWaitOnExit];
    MyProcess.Execute;

    ExitCode := MyProcess.ExitCode;

    WriteLn('Exit code: ', ExitCode);

    if ExitCode = 0 then
      WriteLn('Command succeeded')
    else
      WriteLn('Command failed with code: ', ExitCode);

  finally
    MyProcess.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Running Programs from Your App

You can launch other programs and let them run:

```pascal linenums="1"
program LaunchProgram;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  Process;

var
  MyProcess: TProcess;

begin
  MyProcess := TProcess.Create(nil);

  try
    { Open a URL in the default browser }
    {$IFDEF WINDOWS}
    MyProcess.Executable := 'cmd.exe';
    MyProcess.Parameters.Add('/C');
    MyProcess.Parameters.Add('start https://www.google.com');
    {$ENDIF}

    {$IFDEF UNIX}
    MyProcess.Executable := 'xdg-open';
    MyProcess.Parameters.Add('https://www.google.com');
    {$ENDIF}

    { Don't wait for the program to finish }
    MyProcess.Execute;

    WriteLn('Browser opened!');

  finally
    MyProcess.Free;
  end;

  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Important Notes

- **Exit code 0** usually means success
- **Other exit codes** mean something went wrong (varies by program)
- Use `poWaitOnExit` if you need to wait for the program to finish
- Use `poUsePipes` if you want to capture the output
- Always use `try...finally` to make sure the process is cleaned up

## Common Use Cases

- **Getting file information** - Run `dir` or `ls` and parse the output
- **Running scripts** - Execute batch files, shell scripts, or Python scripts
- **System administration** - Run system commands and check results
- **Opening files** - Use the default program to open documents
- **Converting files** - Call conversion tools and track progress

## The Process Units

You'll need these units:

```pascal
uses
  Process,   { For running external programs }
  Classes,   { For TStringList and other basics }
  SysUtils;  { For string functions }
```

## Next Steps

- Try capturing output from different commands
- Create a program that runs another program and processes its output
- Build error checking based on exit codes