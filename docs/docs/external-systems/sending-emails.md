# Sending Emails (SMTP)

Learn how to send emails from your Free Pascal programs. This is useful for notifications, alerts, or any app that needs to contact users.

## Prerequisites

SMTP email functionality is **not part of FPC's standard RTL**. You need the **Synapse (Ararat)** library.

**Install Synapse:**

1. Clone or download from [Synapse GitHub](https://github.com/geby/synapse)
2. Extract to a folder
3. Add the `lib` directory to your FPC library path in your IDE settings
4. Or use compiler flag: `fpc -Fussynapse_path/lib your_program.pas`

Alternatively, if your distribution provides it (like on Linux), check your package manager for the `synapse` or `libararat` package.

## What You Need

To send emails, you need:

1. An email account
2. The SMTP server address (the mail service host)
3. Your login credentials
4. The email address to send to

Common SMTP servers:

- **Gmail**: `smtp.gmail.com` (port 587)
- **Outlook**: `smtp-mail.outlook.com` (port 587)
- **Yahoo**: `smtp.mail.yahoo.com` (port 587)
- **Custom server**: Ask your email provider for details

## Configuration File (INI)

Instead of hardcoding credentials, create a configuration file. Create a file named `email.ini`:

```ini
[SMTP]
TargetHost=smtp.gmail.com
TargetPort=587
Username=your.email@gmail.com
Password=your.app.password
FullSSL=False
```

Free Pascal has a built-in `IniFiles` unit for reading INI files. Use `TIniFile` class to read configuration values easily.

## Simple Email Example

Here's a basic example sending an email using configuration from an INI file:

```pascal linenums="1"
program SendEmail;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  IniFiles,
  smtpsend,
  ssl_openssl3;

var
  MailText: TStringList;
  IniFile: TIniFile;
  TargetHost, Username, Password: string;

begin
  { Load configuration from INI file }
  IniFile := TIniFile.Create('email.ini');
  try
    TargetHost := IniFile.ReadString('SMTP', 'TargetHost', 'smtp.gmail.com');
    Username := IniFile.ReadString('SMTP', 'Username', '');
    Password := IniFile.ReadString('SMTP', 'Password', '');
  finally
    IniFile.Free;
  end;

  { Create email content }
  MailText := TStringList.Create;
  try
    MailText.Add('From: ' + Username);
    MailText.Add('To: recipient@example.com');
    MailText.Add('Subject: Hello from Free Pascal!');
    MailText.Add('');
    MailText.Add('This is a test email sent from Free Pascal.');
    MailText.Add('');
    MailText.Add('Best regards,');
    MailText.Add('Your Name');

    WriteLn('Sending email...');

    { Use the standalone SendToRaw function from smtpsend module }
    if SendToRaw(Username, 'recipient@example.com', TargetHost, MailText, Username, Password) then
      WriteLn('Email sent successfully!')
    else
      WriteLn('Error sending email');

  finally
    MailText.Free;
  end;

  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Gmail Setup (Important!)

If you're using Gmail, you **cannot** use your regular password. You need to:

1. Turn on **2-Step Verification** in your Google Account
2. Create an **App Password** (not your regular password)
3. Use this app password in your code

[Google Help: Create & use App Passwords](https://support.google.com/accounts/answer/185833)

## HTML Emails

You can send formatted emails using HTML. This example reads configuration from an INI file:

```pascal linenums="1"
program SendHTMLEmail;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  IniFiles,
  smtpsend,
  ssl_openssl3;

var
  MailText: TStringList;
  IniFile: TIniFile;
  TargetHost, Username, Password: string;

begin
  { Load configuration from INI file }
  IniFile := TIniFile.Create('email.ini');
  try
    TargetHost := IniFile.ReadString('SMTP', 'TargetHost', 'smtp.gmail.com');
    Username := IniFile.ReadString('SMTP', 'Username', '');
    Password := IniFile.ReadString('SMTP', 'Password', '');
  finally
    IniFile.Free;
  end;

  MailText := TStringList.Create;
  try
    MailText.Add('From: ' + Username);
    MailText.Add('To: recipient@example.com');
    MailText.Add('Subject: Formatted Email');
    MailText.Add('Content-Type: text/html; charset=UTF-8');
    MailText.Add('');
    MailText.Add('<html>');
    MailText.Add('<body>');
    MailText.Add('<h1>Hello!</h1>');
    MailText.Add('<p>This is a <strong>formatted</strong> email.</p>');
    MailText.Add('<a href="https://example.com">Click here</a>');
    MailText.Add('</body>');
    MailText.Add('</html>');

    WriteLn('Sending HTML email...');

    { Use the standalone SendToRaw function from smtpsend module }
    if SendToRaw(Username, 'recipient@example.com', TargetHost, MailText, Username, Password) then
      WriteLn('Email sent!')
    else
      WriteLn('Error sending email');

  finally
    MailText.Free;
  end;

  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Sending to Multiple Recipients

Send one email to several people. This example reads configuration from an INI file:

```pascal linenums="1"
program SendMultiple;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  IniFiles,
  smtpsend,
  ssl_openssl3;

var
  MailText: TStringList;
  Recipients: TStringList;
  IniFile: TIniFile;
  TargetHost, Username, Password: string;
  i: Integer;

begin
  { Load configuration from INI file }
  IniFile := TIniFile.Create('email.ini');
  try
    TargetHost := IniFile.ReadString('SMTP', 'TargetHost', 'smtp.gmail.com');
    Username := IniFile.ReadString('SMTP', 'Username', '');
    Password := IniFile.ReadString('SMTP', 'Password', '');
  finally
    IniFile.Free;
  end;

  MailText := TStringList.Create;
  Recipients := TStringList.Create;

  try
    { Create email }
    MailText.Add('From: ' + Username);
    MailText.Add('Subject: Team Announcement');
    MailText.Add('');
    MailText.Add('Hello team!');
    MailText.Add('This is an announcement for everyone.');

    { Add recipients (Synapse can handle comma-separated list) }
    Recipients.Add('person1@example.com');
    Recipients.Add('person2@example.com');
    Recipients.Add('person3@example.com');

    WriteLn('Sending emails...');

    { Send to each recipient using the standalone SendToRaw function }
    for i := 0 to Recipients.Count - 1 do
    begin
      WriteLn('Sending to: ', Recipients[i]);
      if SendToRaw(Username, Recipients[i], TargetHost, MailText, Username, Password) then
        WriteLn('  Success!')
      else
        WriteLn('  Failed to send');
    end;

    WriteLn('All emails processed!');

  finally
    MailText.Free;
    Recipients.Free;
  end;

  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Error Handling

Check for problems when sending. This example reads configuration from an INI file:

```pascal linenums="1"
program SendEmailWithErrorHandling;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  IniFiles,
  smtpsend,
  ssl_openssl3;

var
  IniFile: TIniFile;
  TargetHost, Username, Password: string;
  FromAddr, ToAddr: string;
  MailText: TStringList;

begin
  { Load configuration from INI file }
  IniFile := TIniFile.Create('email.ini');
  try
    TargetHost := IniFile.ReadString('SMTP', 'TargetHost', 'smtp.gmail.com');
    Username := IniFile.ReadString('SMTP', 'Username', '');
    Password := IniFile.ReadString('SMTP', 'Password', '');
  finally
    IniFile.Free;
  end;

  FromAddr := Username;
  ToAddr := 'recipient@example.com';
  MailText := TStringList.Create;
  try
    MailText.Add('From: ' + FromAddr);
    MailText.Add('To: ' + ToAddr);
    MailText.Add('Subject: Test Email');
    MailText.Add('');
    MailText.Add('Test message');

    WriteLn('Attempting to send email...');
    WriteLn('From: ', FromAddr);
    WriteLn('To: ', ToAddr);
    WriteLn('Server: ', TargetHost);

    { Use the standalone SendToRaw function from smtpsend module }
    if SendToRaw(FromAddr, ToAddr, TargetHost, MailText, Username, Password) then
    begin
      WriteLn('Success! Email was sent.');
    end
    else
    begin
      WriteLn('Error: Failed to send email.');
      WriteLn('Check:');
      WriteLn('  - SMTP server address and port');
      WriteLn('  - Username and password');
      WriteLn('  - Internet connection');
      WriteLn('  - Firewall settings');
    end;

  finally
    MailText.Free;
  end;

  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Email Format Tips

When building emails, remember:

- The header (From, To, Subject) comes first
- Then a blank line
- Then the body content
- Blank lines in the body are just empty lines in the StringList

## Security Notes

⚠️ **Important:**

- **Never hardcode passwords** in your code
- Use **environment variables** or **config files** to store credentials
- Use **app passwords** (not your main password) for services like Gmail
- Consider using **email service APIs** instead of SMTP for large applications

## Using Environment Variables for Credentials

```pascal
var
  username: string;
  password: string;
begin
  { Get from environment variables instead of hardcoding }
  username := GetEnvironmentVariable('EMAIL_USER');
  password := GetEnvironmentVariable('EMAIL_PASS');

  Mail.Username := username;
  Mail.Password := password;
end;
```

## Common Issues

- **Connection timeout** - Check your server address and port
- **Authentication failed** - Check username and password (use app password for Gmail)
- **Can't resolve host** - Check internet connection and server name
- **SSL certificate errors** - Some servers may need different SSL settings

## Next Steps

- Try sending a simple email to yourself
- Add email notifications to a program
- Create a newsletter system using email
- Implement email verification in your app
