# Dates and Times

!!! info "Format characters"

    The complete list of formatting characters can be found in [`formatchars`](https://www.freepascal.org/daily/doc/rtl/sysutils/formatchars.html) doc.

## How can I get current date and time?

1. Add `sysutils` to the `uses` section.
2. Use `Now` to get current date time.

From here you have choices.

- You can use `DateToStr` to get only the date string from `Now`.
- You can use `TimeToStr` to get only the time string from `Now`.
- You can use `DateTimeToStr` to get the date time string from `Now`.
- You can use `FormatDateTime` to customise the date time string from `Now`.

Here is an example.

```pascal linenums="1"
program DateTimeCurrent;

uses
  SysUtils;

begin

  // Display current date only with the default formatting
  WriteLn('Date: ', DateToStr(Now));

  // Display current time only with the default formatting
  Writeln('Time: ', TimeToStr(Now));

  // Display current timestamp with the default formatting
  Writeln('Now is (default format): ', DateTimeToStr(Now));

  // Display timestamp with custom formatting
  Writeln('Now is (custom format) : ',
          FormatDateTime('yyyy-mm-dd hh:nn:ss.z', Now));
  Writeln('Now is (custom format) : ',
          FormatDateTime('dd-mmm-yy hh:nn AM/PM', Now));

  // Display timestamp in long format -- Tuesday, 9 January 2024 12:08:17 PM
  Writeln('Now is (custom format) : ', FormatDateTime('dddddd tt', Now));

  // Pause console
  ReadLn;
end.
```

## How to get Unix epoch time?

1. In `uses` section add `SysUtils` and `DateUtils`.  
2. Call `DateTimeToUnix(Now);`.

```pascal linenums="1" hl_lines="4 5 11"
program DateTimeUnix;

uses
  SysUtils,
  DateUtils;

var
  unixTime: integer;

begin
  unixTime := DateTimeToUnix(Now);
  Writeln('Current time in Unix epoch time is: ', unixTime);

  // Pause console;
  ReadLn;
end.
```

## Finding days or time between dates

You can use any of the following to find days or time between dates.

- [`YearsBetween`](https://www.freepascal.org/docs-html/rtl/dateutils/yearsbetween.html) - The number of whole years between two `TDateTime` values
- [`MonthsBetween`](https://www.freepascal.org/docs-html/rtl/dateutils/monthsbetween.html) - The number of whole months between two `TDateTime` values
- [`WeeksBetween`](https://www.freepascal.org/docs-html/rtl/dateutils/weeksbetween.html) - The number of whole weeks between two `TDateTime` values
- [`DaysBetween`](https://www.freepascal.org/docs-html/rtl/dateutils/daysbetween.html) - Number of whole days between two `TDateTime` values.
- [`HoursBetween`](https://www.freepascal.org/docs-html/rtl/dateutils/hoursbetween.html) - The number of whole hours between two `TDateTime` values.
- [`MinutesBetween`](https://www.freepascal.org/docs-html/rtl/dateutils/minutesbetween.html) - The number of whole minutes between two `TDateTime` values.
- [`SecondsBetween`](https://www.freepascal.org/docs-html/rtl/dateutils/secondsbetween.html) - The number of whole seconds between two `TDateTime` values.
- [`MilliSecondsBetween`](https://www.freepascal.org/docs-html/rtl/dateutils/millisecondsbetween.html) - The number of whole milliseconds between two `TDateTime` values.

Here is an example of using `SecondsBetween` and `DaysBetween`.

```pascal linenums="1"
program DateTimeBetween;

uses
  SysUtils,
  DateUtils;

var
  diffSec, diffDay: integer;

begin

  // Seconds between 2 times
  diffSec := SecondsBetween(StrToTime('18:30'), StrToTime('07:35'));
  WriteLn('The seconds between 18:30 and 07:35 is: ', diffSec, ' seconds');

  // Days between 2 dates
  diffDay := DaysBetween(StrToDate('09/01/2024'), StrToDate('01/01/2015'));
  WriteLn('The days difference between 2024-01-05 and 2015-01-01 is: ', diffDay, ' days');

  // Seconds between 2 dates
  // Casting diffsec to float for nicer formatting
  diffSec := SecondsBetween(StrToDate('09/01/2024'), StrToDate('01/01/2015'));
  WriteLn('The seconds between 2024-01-05 and 2015-01-01 is: ', Format('%n', [diffSec + 0.0]),' seconds');

  ReadLn;
end.
```

## Simple profiling a section of a program

1. Place the `GetTickCount64` before and after a specific section of code.
2. Calculate the elapsed time in milliseconds by subtracting the initial value from the final value obtained from `GetTickCount64`.

!!! warning

    `GetTickCount64` returns an increasing clock tick count in milliseconds. It is useful for time measurements, but ==no assumptions should be made as to the interval between the ticks==.

    [https://www.freepascal.org/daily/doc/rtl/sysutils/gettickcount64.html](https://www.freepascal.org/daily/doc/rtl/sysutils/gettickcount64.html)


Here is an example.

```pascal linenums="1" hl_lines="12 18 21"
program DateTimeBenchmark;

uses
  SysUtils;

var
  startTime, endTime: QWord;

begin

  // Get start time.
  startTime := GetTickCount64;

  // Simulate a long running task
  Sleep(1000);

  // Get end time
  endTime := GetTickCount64;

  // Display time elapsed
  Writeln('Time elapsed: ', (endTime-startTime), ' ms');

  // Pause console
  ReadLn;
end.
```

## How can I compare two dates?

Use the comparison operators (`<`, `>`, `<=`, `>=`, `=`) to compare two `TDateTime` values.

Consider the following example.

```pascal linenums="1" hl_lines="15"
program DateTimeComparison;

uses
  SysUtils,
  DateUtils;

var
  surveyReleaseDate: TDateTime;

begin

  // Set survey release date
  surveyReleaseDate := StrToDate('15/12/2024');

  if surveyReleaseDate < Now then
  begin
    WriteLn('You can release survey results now');
    WriteLn('You''re behind by ', DaysBetween(Now, surveyReleaseDate), ' days.');
  end
  else
  begin
    WriteLn('You CANNOT release survey results! Wait!');
    WriteLn('You can release results in ', DaysBetween(Now, surveyReleaseDate), ' days.');
  end;

  // Pause console
  ReadLn;
end.
```

## How do I parse a string date in dd-mmm-yy format?

Use [`ScanDateTime`](https://www.freepascal.org/docs-html/rtl/dateutils/scandatetime.html) from `DateUtils`.

Take a look at this example.

```pascal linenums="1" hl_lines="5 20" 
program ParseDate;

uses
  SysUtils,
  DateUtils;

var
  dateString: string;
  parsedDate: TDateTime;

begin

  // date to parse
  dateString := '03-MAR-24';

  // default value of parsed date. If conversion fails, use this value
  parsedDate := Default(TDateTime);

  try
    parsedDate := ScanDateTime('dd?mmm?yy', dateString);
    // Now dateValue contains the parsed date, else ScanDateTime will raise an exception

    // Use FormatDateTime to format the parsed date as needed
    WriteLn('Parsed date                : ', DateToStr(parsedDate));
    WriteLn('Parsed date (custom format): ', FormatDateTime('dddddd tt', parsedDate));
    WriteLn('Parsed date (custom format): ', FormatDateTime('yyyy-mm-dd hh:nn:ss', parsedDate));
  except
    on E: EConvertError do
      WriteLn('Invalid date string');
  end;

  // Pause console
  Readln;
end.
```