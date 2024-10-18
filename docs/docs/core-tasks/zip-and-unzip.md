# Zip and Unzip Files

!!! Note
    Visit the official docs for more info.

    - [https://wiki.lazarus.freepascal.org/paszlib](https://wiki.lazarus.freepascal.org/paszlib)
    - [https://www.freepascal.org/docs-html/fcl/zipper/index.html](https://www.freepascal.org/docs-html/fcl/zipper/index.html)

## Zip Files - Simple

```pascal linenums="1"
program ZipEx01;

{$mode objfpc}{$H+}{$J-}

uses
  zipper;

var
  zip: TZipper;

begin
  zip := TZipper.Create;
  try
    zip.FileName := 'simple.zip';
    zip.Entries.AddFileEntry('file1.txt');
    zip.Entries.AddFileEntry('file2.txt');
    zip.ZipAllFiles;
  finally
    zip.Free;
  end;
  WriteLn('File zipped successfully.');
end.
```

## Zip Files - Command Line Input

!!! Note

    The following example was adapted from the official doc: [https://wiki.lazarus.freepascal.org/paszlib](https://wiki.lazarus.freepascal.org/paszlib).


```pascal linenums="1"
program ZipEx02;

{$mode objfpc}{$H+}{$J-}

// Usage:
// ZipEx02 newzip.zip file1.txt file2.txt

uses
  Zipper;

var
  zip: TZipper;
  index: integer;

begin
  zip := TZipper.Create;
  try
    // Define the file name of the zip file to be created
    zip.FileName := ParamStr(1);
    for index := 2 to ParamCount do
      // First argument: the names of the files to be included in the zip
      // Second argument: the name of the file as it appears in the zip and
      // later in the file system after unzipping
      zip.Entries.AddFileEntry(ParamStr(index), ParamStr(index));
    // Execute the zipping operation and write the zip file.
    zip.ZipAllFiles;
  finally
    zip.Free;
  end;
end.
```

**Usage**

```bash
$ ./ZipEx02.exe newzip.zip file1.txt file2.txt
```

## Unzip a Zip File 

```pascal linenums="1"
program UnzipEx01;

{$mode objfpc}{$H+}{$J-}

uses
  zipper;

var
  unZip: TUnZipper;

begin
  unZip := TUnZipper.Create;
  try
    unZip.FileName := 'simple.zip';
    unZip.OutputPath := 'output_folder';
    unZip.UnZipAllFiles;
  finally
    unZip.Free;
  end;
  WriteLn('File unzipped successfully.');
end.
```