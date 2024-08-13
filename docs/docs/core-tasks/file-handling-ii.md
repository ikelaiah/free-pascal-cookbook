# File Handling II

More snippets on handling files using Free Pascal.

## Listing specific columns in a CSV file - TCSVDataSet

Here is an example of reading first two columns in a CSV file using `TCSVDataset`.

```pascal linenums="1"
program TCSVDatasetGetSpecificCols;

{
 An example of listing the content of first two columns in a CSV file
 using TCSVDataset.
}

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  streamex,
  bufstream,
  csvdataset;

  // A routine to list first two columns of a CSV file
  procedure ReadCSV(filename: string;
                    delimiter: char = ',';
                    isFirstRowFieldName: boolean = False);
  var
    fileStream: TFileStream;
    buffStream: TReadBufStream;
    csvDataset: TCSVDataset;
    lineNo: int64;
  begin
    fileStream := TFileStream.Create(filename, fmOpenRead);
    try
      buffStream := TReadBufStream.Create(fileStream, 65536);
      try
        csvDataset := TCSVDataset.Create(nil);
        try

          // Assign a valid delimiter
          csvDataset.CSVOptions.Delimiter := delimiter;

          // Is the first line field names?
          // If yes, first row will be excluded when listing rows
          csvDataset.CSVOptions.FirstLineAsFieldNames := isFirstRowFieldName;

          // Load CSV from the stream
          csvDataset.LoadFromCSVStream(buffStream);

          // Move to first record
          csvDataset.First;

          lineNo := 1;

          while not csvDataset.EOF do
          begin
            // Get the values of the first two fields here and list them.
            WriteLn(Format('row %d: %s, %s',
                           [lineNo,
                            csvDataset.Fields[0].AsString,
                            csvDataset.Fields[1].AsString]));

            // Move to next
            csvDataset.Next;

            // Increment line no
            lineNo := lineNo + 1;
          end;

        finally
          csvDataset.Free;
        end;
      finally
        buffStream.Free;
      end;
    finally
    end;
    fileStream.Free;
  end;

var
  filename: string;

begin

  filename := ParamStr(1);
  if not FileExists(filename) then
  begin
    WriteLn('Cannot find file.');
    Exit;
  end;

  ReadCSV(filename, ';', False);
end.
```

## Listing specific columns in a CSV file - TCSVDocument

Here is an example of reading first two columns in a CSV file using `TCSVDocument`.

```pascal linenums="1"
program TCSVDocumentGetSpecificCols;


{
 An example of listing the content of first two columns in a CSV file
 using TCSVDocument.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  csvdocument,
  streamex,
  bufstream;

  procedure ReadCSV(filename: string; delimiter: char);
  var
    fileStream: TFileStream;
    buffStream: TReadBufStream;
    csvReader: TCSVDocument;
    index, totalLines: int64;
  begin
    totalLines := 0;
    fileStream := TFileStream.Create(filename, fmOpenRead);
    try
      buffStream := TReadBufStream.Create(fileStream, 65536);
      try
        csvReader := TCSVDocument.Create;
        try
          // Assign a delimiter
          csvReader.Delimiter := delimiter;

          // Assign a source stream.
          csvReader.LoadFromStream(buffStream, 65536);

          // Get total lines for iteration.
          totalLines := csvReader.RowCount;

          // Print the values of first two columns from the CSV file.
          for index := 0 to totalLines-1 do
          begin
            WriteLn(Format('row %d: %s, %s', [(index + 1),
                                              csvReader.Cells[0, index],
                                              csvReader.Cells[1, index]]));
          end;

        finally
          csvReader.Free;
        end;
      finally
        buffStream.Free;
      end;
    finally
    end;
    fileStream.Free;
  end;

var
  filename: string;

begin
  filename := ParamStr(1);
  if not FileExists(filename) then
  begin
    WriteLn('Cannot find file.');
    Exit;
  end;

  ReadCSV(filename, ';');
end.
```

## Split text file into chunks of 1 Mbytes - TFileStream

This snippet splits a large text file into smaller chunks without breaking lines or paragraphs. Here's a summary of the program's functionality:

- Procedure `SaveChunkToFile`: Handles creating new files for each chunk and writing the chunk data to these files.
- **Constants and Variables**:
    - `defaultChunkSize`: The size of each chunk, set to 1 MB.
    - `fileStream`: Used to read from the input file.
    - `buffer`: A memory buffer to hold chunk data.
    - `bytesRead`, `totalBytesRead`, `chunkSize`, `lineBreakPos`, `chunkIndex`: Variables to track file reading and processing.
- **Main Logic**:
    - The program checks if the input file exists.
    - It reads the file in chunks, finds the last newline in each chunk, adjusts the file pointer if necessary, and writes each chunk to a new file.
    - The loop continues until the entire text file is processed.

```pascal linenums="1"
program TFileStreamSplitFile;

{
 This program splits a text file based on a chunkSize.
 The algorithm ensures it won't split the text in the middle of a line/paragraph.

 1. Open the file and allocate memory bufers for reading chunks of data.

 2. Read the file in chunks and locate the last `\n` character in the chunk.
    Once it locates the last `\n` in the chunk, move the file pointer back to include
    that byte and any preceding bytes of the partial line in the next chunk's read operation.

 3. Repeat - read and parse the remainder.

 4. Once parsing is complete, close the file and free any allocated memory (to prevent memory leaks).
}

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  SysUtils,
  Classes,
  bufstream;

  procedure SaveChunkToFile(const filename: string; const chunkData: pansichar;
  const dataSize: integer; const chunkIndex: integer);
  var
    chunkFile: TFileStream;
  begin
    // Create a new file for the chunk
    chunkFile := TFileStream.Create(filename + '-chunk-' + IntToStr(ChunkIndex) +
      '.txt', fmCreate);
    try
      // Write the chunk data to the chunk file
      chunkFile.WriteBuffer(chunkData^, dataSize);
    finally
      chunkFile.Free;
    end;
  end;

const
  defaultChunkSize: integer = 1048576; // 1 MB in bytes

var
  fileStream: TFileStream;
  buffer: pansichar;
  bytesRead, totalBytesRead, chunkSize, lineBreakPos, chunkIndex: int64;

begin

  if not FileExists(ParamStr(1)) then
  begin
    WriteLn('Please spefcify a valid text file.');
    Exit;
  end;

  chunkSize := defaultChunkSize * 1;

  // Open the file for reading
  fileStream := TFileStream.Create(ParamStr(1), fmOpenRead);
  try
    // Allocate memory buffer for reading chunks
    // Ref: https://www.freepascal.org/docs-html/rtl/system/getmem.html
    GetMem(buffer, chunkSize);
    try
      totalBytesRead := 0;
      chunkIndex := 0;

      // Read and parse chunks of data until EOF
      while totalBytesRead < fileStream.Size do
      begin
        bytesRead := fileStream.Read(buffer^, chunkSize);
        Inc(totalBytesRead, bytesRead);

        // Find the position of the last newline character in the chunk
        lineBreakPos := BytesRead;
        while (lineBreakPos > 0) and (Buffer[lineBreakPos - 1] <> #10) do
          Dec(lineBreakPos);

        { Now, must ensure that if the last byte read in the current chunk
          is not a newline character, the file pointer is moved back to include
          that byte and any preceding bytes of the partial line in the next
          chunk's read operation.

          Also, no need to update the BytesRead variable in this context because
          it represents the actual number of bytes read from the file, including
          any partial line that may have been included due to moving the file
          pointer back.
          Ref: https://www.freepascal.org/docs-html/rtl/classes/tstream.seek.html}
        if lineBreakPos < bytesRead then
          fileStream.Seek(-(bytesRead - lineBreakPos), soCurrent);

        // Write the chunk data to a file using the separate procedure
        SaveChunkToFile('output', buffer, lineBreakPos, chunkIndex);

        // Display user feedback
        WriteLn('Chunk ', chunkIndex, ', Total bytes read:', IntToStr(totalBytesRead));

        // Increase chunk index - a counter
        Inc(chunkIndex);
      end;
    finally
      // Free the memory buffer
      FreeMem(buffer);
    end;
  finally
    // Close the file
    fileStream.Free;
  end;
end.
```