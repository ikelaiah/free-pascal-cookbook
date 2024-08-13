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
ReadLn;
```

- The `WriteLn` function prints text on the console.
- The `ReadLn` in the code prevents the console from being closed automatically.


2. Add the following compiler directives after the `program` declaration. 

```pascal
{$mode objfpc}{$H+}{$J-}
```

!!! Note
    Make sure to have this line in all your Object Pascal codes.

    `{$mode objfpc}{$H+}{$J-}`

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

Press ++f9++ to run the compile and run the program.

You should be able to see a console open with `Hello World` in it.

Press the ++enter++ key to exit the progam, which also closes the console.


## Using the CLI

!!! Note
    This section assumes you have correctly set up the Free Pascal Compiler and the `fpc` is in your `PATH`. 

### Create a `.pas` File

1. Launch your favourite text editor
2. Create a new file and put the following snippet in it.

```pascal linenums="1"
{$mode objfpc}{$H+}{$J-} 

begin
    WriteLn('Hello, World!');
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

    If running `fpc` from CLI doesn't work, try one of the following options.

      1. Supply the full path to the `fpc`.
      2. Put the `fpc/bin/<architecture>` to your `PATH` then compile and run again.
      3. Consider using [Lazarus IDE](https://www.lazarus-ide.org) instead.
      4. Are you a [VSCode](https://code.visualstudio.com) or [VSCodium](https://vscodium.com) user? Make sure to setup [Pascal by Allesandro Fragnani](https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal) properly.
      5. Have you considered [OmniPascal](https://www.omnipascal.com)?

#### Linux CLI

On Linux, compile and run as follows.

```bash
$ fpc HelloWorld.pas && ./HelloWorld
```