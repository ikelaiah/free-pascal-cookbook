# Working with Environment Variables and Configuration

Configuration is how your program knows what to do without hardcoding values. Learn how to use environment variables and config files to store settings.

## What Are Environment Variables?

Environment variables are values stored by your operating system. Every program can access them. They're perfect for:

- API keys and passwords
- Server addresses
- Feature flags
- File paths

## Reading Environment Variables

Here's how to get environment variable values:

```pascal linenums="1"
program ReadEnvVars;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils;

var
  userName: string;
  userHome: string;
  pathVar: string;
  apiKey: string;

begin
  { Read some standard environment variables }
  userName := GetEnvironmentVariable('USERNAME');
  userHome := GetEnvironmentVariable('HOME');  { On Unix/Mac }
  pathVar := GetEnvironmentVariable('PATH');

  WriteLn('Username: ', userName);
  WriteLn('Home directory: ', userHome);
  WriteLn('');

  { Read custom variables }
  apiKey := GetEnvironmentVariable('MY_API_KEY');
  if apiKey = '' then
    WriteLn('MY_API_KEY is not set')
  else
    WriteLn('API Key found: ', apiKey);

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Setting Environment Variables

### On Windows

Open Command Prompt and run:
```
set MY_API_KEY=my-secret-key-123
set DB_HOST=localhost
set DB_PORT=5432
```

Then run your program from the same command prompt.

### On Mac/Linux

Open Terminal and run:
```bash
export MY_API_KEY=my-secret-key-123
export DB_HOST=localhost
export DB_PORT=5432
```

Then run your program from the same terminal.

### Using .env Files

Create a file named `.env` in your project:

```
API_KEY=your-secret-key
DB_HOST=localhost
DB_PORT=5432
DB_USER=admin
DEBUG=true
```

Then load it in your program:

```pascal linenums="1"
program LoadEnvFile;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils;

procedure LoadEnvFile(filename: string);
var
  envFile: TStringList;
  i: integer;
  line: string;
  key, value: string;
  eqPos: integer;
begin
  envFile := TStringList.Create;
  try
    envFile.LoadFromFile(filename);

    for i := 0 to envFile.Count - 1 do
    begin
      line := envFile[i];

      { Skip empty lines and comments }
      if (line = '') or (line[1] = '#') then
        Continue;

      { Find the equals sign }
      eqPos := Pos('=', line);
      if eqPos > 0 then
      begin
        key := Trim(Copy(line, 1, eqPos - 1));
        value := Trim(Copy(line, eqPos + 1, Length(line)));

        { Set as environment variable }
        SetEnvironmentVariable(key, value);
      end;
    end;

  finally
    envFile.Free;
  end;
end;

begin
  { Load variables from .env file }
  LoadEnvFile('.env');

  { Now you can read them }
  WriteLn('API Key: ', GetEnvironmentVariable('API_KEY'));
  WriteLn('DB Host: ', GetEnvironmentVariable('DB_HOST'));
  WriteLn('DB Port: ', GetEnvironmentVariable('DB_PORT'));

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Configuration Files (INI Format)

INI files are simple configuration format:

```ini
[Database]
Host=localhost
Port=5432
Username=admin
Password=secret

[API]
Endpoint=https://api.example.com
Timeout=30
Retries=3

[Features]
Debug=true
LogLevel=info
```

Reading INI files:

```pascal linenums="1"
program ReadINI;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  IniFiles;

var
  IniFile: TIniFile;
  dbHost: string;
  dbPort: integer;
  apiEndpoint: string;
  debugMode: boolean;

begin
  { Create INI file object }
  IniFile := TIniFile.Create('config.ini');
  try
    { Read string values }
    dbHost := IniFile.ReadString('Database', 'Host', 'localhost');
    apiEndpoint := IniFile.ReadString('API', 'Endpoint', 'https://api.example.com');

    { Read integer values }
    dbPort := IniFile.ReadInteger('Database', 'Port', 5432);

    { Read boolean values }
    debugMode := IniFile.ReadBool('Features', 'Debug', False);

    WriteLn('Database Host: ', dbHost);
    WriteLn('Database Port: ', dbPort);
    WriteLn('API Endpoint: ', apiEndpoint);
    WriteLn('Debug Mode: ', debugMode);

  finally
    IniFile.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Writing Configuration Files

You can also create/update INI files:

```pascal linenums="1"
program WriteINI;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  IniFiles;

var
  IniFile: TIniFile;

begin
  { Create or open config file }
  IniFile := TIniFile.Create('config.ini');
  try
    { Write values }
    IniFile.WriteString('Database', 'Host', 'db.example.com');
    IniFile.WriteInteger('Database', 'Port', 5432);
    IniFile.WriteString('Database', 'Username', 'admin');
    IniFile.WriteBool('Features', 'Debug', True);

    WriteLn('Configuration saved to config.ini');

  finally
    IniFile.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Configuration Class

Create a helper class to manage configuration:

```pascal linenums="1"
program ConfigClass;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  IniFiles;

type
  { Configuration helper }
  TConfig = class
  private
    FConfigFile: TIniFile;
  public
    constructor Create(filename: string);
    destructor Destroy; override;
    function GetString(section, key, default: string): string;
    function GetInteger(section, key: string; default: integer): integer;
    function GetBool(section, key: string; default: boolean): boolean;
    procedure SetString(section, key, value: string);
    procedure SetInteger(section, key: string; value: integer);
    procedure SetBool(section, key: string; value: boolean);
  end;

constructor TConfig.Create(filename: string);
begin
  FConfigFile := TIniFile.Create(filename);
end;

destructor TConfig.Destroy;
begin
  FConfigFile.Free;
  inherited;
end;

function TConfig.GetString(section, key, default: string): string;
begin
  Result := FConfigFile.ReadString(section, key, default);
end;

function TConfig.GetInteger(section, key: string; default: integer): integer;
begin
  Result := FConfigFile.ReadInteger(section, key, default);
end;

function TConfig.GetBool(section, key: string; default: boolean): boolean;
begin
  Result := FConfigFile.ReadBool(section, key, default);
end;

procedure TConfig.SetString(section, key, value: string);
begin
  FConfigFile.WriteString(section, key, value);
end;

procedure TConfig.SetInteger(section, key: string; value: integer);
begin
  FConfigFile.WriteInteger(section, key, value);
end;

procedure TConfig.SetBool(section, key: string; value: boolean);
begin
  FConfigFile.WriteBool(section, key, value);
end;

{ Using the config class }
var
  Config: TConfig;
  dbHost: string;
  dbPort: integer;

begin
  Config := TConfig.Create('app.ini');
  try
    { Read configuration }
    dbHost := Config.GetString('Database', 'Host', 'localhost');
    dbPort := Config.GetInteger('Database', 'Port', 5432);

    WriteLn('Host: ', dbHost);
    WriteLn('Port: ', dbPort);

    { Update configuration }
    Config.SetString('Database', 'Host', 'new-host.example.com');
    Config.SetInteger('Database', 'Port', 3306);

    WriteLn('Configuration updated');

  finally
    Config.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Priorities for Configuration

Usually you want this order:

1. **Command-line arguments** (highest priority)
2. **Environment variables**
3. **Configuration file**
4. **Built-in defaults** (lowest priority)

```pascal
var
  apiKey: string;
begin
  { Try command-line argument first }
  apiKey := GetEnvironmentVariable('API_KEY_ARG');

  { Then try environment variable }
  if apiKey = '' then
    apiKey := GetEnvironmentVariable('API_KEY');

  { Then try config file }
  if apiKey = '' then
    apiKey := Config.GetString('API', 'Key', '');

  { Finally use default }
  if apiKey = '' then
    apiKey := 'default-key';

  WriteLn('Using API key: ', apiKey);
end;
```

## Security Tips

⚠️ **Important:**

- **Never commit .env files** to git (add to .gitignore)
- **Never hardcode passwords** in source code
- **Use strong values** for secrets and API keys
- **Different configs for different environments**:
  - `.env.dev` for development
  - `.env.prod` for production
- **Rotate credentials** regularly
- **Log configuration loading** but not sensitive values

## Example .gitignore

```
.env
.env.local
*.ini
config.local.ini
secrets/
```

## Standard Environment Variables

Your program can also use these standard variables:

| Variable | What It Is |
|----------|-----------|
| `PATH` | Paths where the OS finds programs |
| `HOME` | User's home directory |
| `USERNAME` | Current user's name |
| `TEMP` | Temporary files directory |
| `COMPUTERNAME` | Computer's name |
| `OS` | Operating system |

## Next Steps

- Create a .env file for your project
- Use a configuration file instead of hardcoding values
- Build a configuration system for your app
- Use environment variables for secrets