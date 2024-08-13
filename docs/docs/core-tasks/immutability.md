# Immutability

## How do I make `const` variables constant?

Add a global directive `{$J-}` to your main pascal file.


## Why is `const` section writeable in by default?

In Turbo Pascal 7.0, you could only assign values in the `const` section, not in `var` section.

As [the official docs](https://www.freepascal.org/docs-html/ref/refse10.html) mentions, by default `const` is writeable (that is `{$J+}`) to keep Turbo Pascal compatibility.

>  It's a relic of old Delphi and Object Pascal and thus `{$J+}` is the default to allow old programs to compile, but it isn't required anymore
> 
> chsh -s /usr/bin/pwsh — 13/12/2023 12:09

Consider the following program. By the default `{$J+}`, you can change the gravity of the Earth, with the gravity of the Moon!

```pascal linenums="1"
program Immutability;

{$mode objfpc}{$H+}

const
  gravityEarth:Real = 9.81;

begin
  gravityEarth := 1.625; // Look, we are on the moon.
  WriteLn('The gravity on the Earth is ', gravityEarth:0:2);
end.
```

To make `const` variables behaving like `const` variables in modern programming languages, use `{$J-}`


```pascal linenums="1"
program Immutability;

{$mode objfpc}{$H+}{$J-}

const
  gravityEarth:Real = 9.81;

begin
  // With {$J-}, the following line will not compile
  // gravityEarth := 1.625;
  WriteLn('The gravity on the Earth is ', gravityEarth:0:2);
end.
```