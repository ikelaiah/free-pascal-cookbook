# Hello, World! 

How would you like to do your `Hello World`?

- [I'm using the Lazarus IDE](#using-the-lazarus-ide)
- [I'm using the CLI](#using-the-cli)

## Using the Lazarus IDE

!!! Note

    This section assumes you have correctly set up the following for your OS.

    1. The Free Pascal Compiler.
    2. The Lazarus IDE.

### Create a Project

1. Launch [Lazarus IDE](https://www.lazarus-ide.org).
2. Create a new Project.
      - On the top menu bar, click `Project -> Simple Program -> OK`.
3. Save this Project.
      - Click `Project -> Save Project`. 
      - Save the Project file as `HelloWorld.lpi` in a new folder. 
      - **Note**: Lazarus will save the main source file as `HelloWorld.lpr`.

You will see a simple program in the **Source Editor** window. The `program`'s name will be the same as the Project's name, as shown below.

```pascal linenums="1"
program HelloWorld;

begin
end.
```

### Add Code

1. Now insert the following lines between `begin` and `end.`.

```pascal
WriteLn('Hello, World!');
ReadLn; // Add this to pause the program and see the output
```

- The `WriteLn` function prints text on the console.
- The `ReadLn` waits for you to press Enter, keeping the console window open so you can see the "Hello, World!" message.


2. Add the following compiler directives after the `program` declaration. 

```pascal
{$mode objfpc}{$H+}{$J-}
```

!!! Note
    This line `{$mode objfpc}{$H+}{$J-}` is a common setup for Free Pascal projects. It tells the compiler to use modern Object Pascal features and settings. It's good practice to include it.

Your final code would look as follows.

```pascal hl_lines="3 6 7" linenums="1"
program HelloWorld;

{$mode objfpc}{$H+}{$J-} // Add this line in your object pascal codes.

begin
  WriteLn('Hello, World!');
  ReadLn;
end.
```

3. Press ++ctrl+s++ to save the code.

### Compile & Run

Press ++f9++ to compile and run the program.

You should be able to see a console window open with `Hello, World!` displayed.

Lazarus IDE: Press the ++enter++ key to exit the program, which also closes the console.


## Using the CLI

!!! Note
    This section assumes you have correctly set up the Free Pascal Compiler and the `fpc` is in your `PATH`. 

### Create a `.pas` File & Add Code

1. Launch your favourite text editor.
2. Create a new file and put the following snippet in it.

```pascal linenums="1"
program HelloWorldCLI; // It's good practice to name your program
{$mode objfpc}{$H+}{$J-} 

begin
    WriteLn('Hello, World!');
    ReadLn; // Add this to pause the program and see the output
end.
```

3. Save it as `HelloWorld.pas`.

### Compile & Run

#### Windows CLI

On Windows, compile and run as follows.

```bash
> fpc HelloWorld.pas && HelloWorld.exe
```


!!! Tip

    If running `fpc` from the Command Prompt (CLI) doesn't work, here are a few things to check:

      1. **Full Path**: Try using the full path to `fpc.exe` (e.g., `C:\FPC\3.2.2\bin\i386-win32\fpc.exe HelloWorld.pas`).
      2. **PATH Variable**: Ensure the directory containing `fpc.exe` (e.g., `C:\FPC\3.2.2\bin\i386-win32\`) is added to your system's `PATH` environment variable. You might need to restart your Command Prompt after updating PATH.
      3. **Lazarus IDE**: If CLI is tricky, using the [Lazarus IDE](https://www.lazarus-ide.org) (as shown above) is often easier for beginners.
      4. **VSCode/VSCodium Users**: If you're using [VSCode](https://code.visualstudio.com) or [VSCodium](https://vscodium.com), make sure the [Pascal extension by Alessandro Fragnani](https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal) is set up correctly to find the compiler.
      5. **OmniPascal**: For a more integrated VSCode experience, you might also look into [OmniPascal](https://www.omnipascal.com).

#### Linux CLI

On Linux, compile and run as follows.

```bash
$ fpc HelloWorld.pas && ./HelloWorld
```
