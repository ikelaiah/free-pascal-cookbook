# Conditional Compilation


## How can I compile conditionally?

### Use [`{$IFDEF}`](https://www.freepascal.org/docs-html/prog/progsu31.html)

You can use [`$DEFINE`](https://www.freepascal.org/docs-html/prog/progsu11.html) to define a symbol to compile your program conditionally.

For example.

```pascal
{$DEFINE name}
```

Then, use [`$IFDEF name`](https://www.freepascal.org/docs-html/prog/progsu31.html) to start the conditional compilation.
Use [`$ENDIF some ignored comments`](https://www.freepascal.org/docs-html/prog/progsu16.html) to end the conditonal compilation.

```pascal
 {$IFDEF name}

 // More code ...

 {$ENDIF name}
```

You can combine multiple `{$IFDEF}`s too. See the following example (taken from [https://wiki.freepascal.org/$IF](https://wiki.freepascal.org/$IF)).

```pascal linenums="1"
//Before you need write these conditions to check some conditions:
{$DEFINE SOMETHING}
{$DEFINE SOMETHINGELSE}
{$IFDEF SOMETHING} //Combine $IFDEF to check multiple conditions
  {$IFDEF SOMETHINGELSE}
    {$ModeSwitch advancedrecords}
  {$ENDIF}
{$ENDIF}
```

### Use [`{$IF}`](https://www.freepascal.org/docs-html/prog/progsu29.html)

[`{$IF}`](https://www.freepascal.org/docs-html/prog/progsu29.html) allows you to write complex conditions that cannot be done using `{$ifdef aname}`.

See the following example from [https://wiki.freepascal.org/\$IF](https://wiki.freepascal.org/$IF). You can see two local directives (`$DEFINE` and `$UNDEF`) along with `and` and `or` logic operators in it.

```pascal linenums="1"
{$IF defined(SOMETHING) and defined(SOMETHINGELSE)}//simple and readabl instead of union {$IFDef}`s
  {$ModeSwitch advancedrecords}
{$ENDIF} 

{$IF defined(somthing) or defined(somethingelse)}
  //Whatever you need!
{$ENDIF}

{$IF undefined(what) and defined(somethingelse)}
  //Just for note, Another usage!
{$ENDIF}
```

**References**

- [Lazarus - Easy trick on how to use {$IFDEF DEBUG} for simple debugging](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/lazarus-easy-trick-on-how-to-use-ifdef-debug-for-simple-debugging/) by Hans Luijten on [Tweaking4All](https://www.tweaking4all.com).
- [Conditional Compilation `{$IFDEF}` - Free Pascal Wiki](https://wiki.freepascal.org/Conditional_compilation)
- [Conditional Compilation with `{$IF}` - Free Pascal Wiki](https://wiki.freepascal.org/$IF)

## Useful symbols for conditional compilation

Here is an example contributed by [𝓚𝓸𝓭𝓮𝓩𝔀𝓮𝓻𝓰 🇩🇪](https://discord.com/channels/570025060312547359/570025060312547361/1193531999542063134) (Unofficial Free Pascal Discord).

```pascal linenums="1"
{$IF Defined(DCC) or Defined(VER210) or Defined(VER200) or Defined(VER190) or Defined(VER185) or Defined(VER180) or Defined(VER170) or Defined(VER160) or Defined(VER150) or Defined(VER140) or Defined(VER130) or Defined(VER120) or Defined(VER100) or Defined(VER90) or Defined(VER80)}
  {$DEFINE Delphi} { Delphi }
{$IFEND}

{$IF Defined(DELPHI) and Declared(CompilerVersion) and (CompilerVersion >= 25)}
  {$LEGACYIFEND ON}
{$IFEND}

{$IF Defined(FPC)}
  {$DEFINE Lazarus} { Lazarus and Free Pascal }
{$IFEND}

{$IF Defined(DELPHI) and Declared(CompilerVersion) and (CompilerVersion >= 23)}
  {$DEFINE NameSpace} { Delphis NameSpace feature (eg Winapi.Windows instead of Windows) }
{$IFEND}

{$IF Defined(DELPHI) and Declared(CompilerVersion) and (CompilerVersion >= 20)}
  {$DEFINE UniCode} { Delphis UniCode support }
{$IFEND}

{$IF Defined(WIN32) or Defined(WIN64) or Defined(MSWindows)}
  {$DEFINE Windows} { We are on Windows }
{$IFEND Windows}

{$IF Defined(FPC) and Declared(FPC_VERSION) and (FPC_VERSION >= 3)}
  {$DEFINE UniCode} { FreePascal UniCode support }
{$IFEND}
```

And here's an example of how you might use these symbols.

```pascal linenums="1"
begin
{$IFDEF Delphi}
    // Specific code for Delphi mode
{$ENDIF}

{$IFDEF Lazarus}
    // Specific code for Lazarus and Free Pascal
{$ENDIF}

{$IFDEF Namespace}
    // Specific code for Delphi's namespace
{$ENDIF}

{$IFDEF Windows}
    // Specific code for Windows
{$ENDIF}

{$IFDEF UniCode}
    // Code relies on Unicode
{$ENDIF}
end.


```