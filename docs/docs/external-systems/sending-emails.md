# Sending Emails (SMTP)

Learn how to send emails from your Free Pascal programs. This is useful for notifications, alerts, or any app that needs to contact users.

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

## Simple Email Example

Here's a basic example sending an email:

```pascal linenums="1"
program SendEmail;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  smtpsend,
  ssl_openssl;

var
  Mail: TSMTPSend;
  MailText: TStringList;

begin
  { Create email content }
  MailText := TStringList.Create;
  try
    MailText.Add('From: your.email@gmail.com');
    MailText.Add('To: recipient@example.com');
    MailText.Add('Subject: Hello from Free Pascal!');
    MailText.Add('');
    MailText.Add('This is a test email sent from Free Pascal.');
    MailText.Add('');
    MailText.Add('Best regards,');
    MailText.Add('Your Name');

    { Create SMTP client }
    Mail := TSMTPSend.Create;
    try
      { Set server details }
      Mail.TargetHost := 'smtp.gmail.com';
      Mail.TargetPort := '587';
      Mail.Username := 'your.email@gmail.com';
      Mail.Password := 'your.app.password';  { Use app password, not regular password }
      Mail.FullSSL := False;  { Use STARTTLS }

      WriteLn('Connecting to email server...');

      { Try to send email }
      if Mail.SendToRaw('your.email@gmail.com', 'recipient@example.com', MailText) then
        WriteLn('Email sent successfully!')
      else
        WriteLn('Error: ', Mail.EnhancedStatusCode);

    finally
      Mail.Free;
    end;

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

You can send formatted emails using HTML:

```pascal linenums="1"
program SendHTMLEmail;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  smtpsend,
  ssl_openssl;

var
  Mail: TSMTPSend;
  MailText: TStringList;

begin
  MailText := TStringList.Create;
  try
    MailText.Add('From: your.email@gmail.com');
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

    Mail := TSMTPSend.Create;
    try
      Mail.TargetHost := 'smtp.gmail.com';
      Mail.TargetPort := '587';
      Mail.Username := 'your.email@gmail.com';
      Mail.Password := 'your.app.password';
      Mail.FullSSL := False;

      WriteLn('Sending HTML email...');

      if Mail.SendToRaw('your.email@gmail.com', 'recipient@example.com', MailText) then
        WriteLn('Email sent!')
      else
        WriteLn('Error: ', Mail.EnhancedStatusCode);

    finally
      Mail.Free;
    end;

  finally
    MailText.Free;
  end;

  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Sending to Multiple Recipients

Send one email to several people:

```pascal linenums="1"
program SendMultiple;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  smtpsend,
  ssl_openssl;

var
  Mail: TSMTPSend;
  MailText: TStringList;
  Recipients: TStringList;

begin
  MailText := TStringList.Create;
  Recipients := TStringList.Create;

  try
    { Create email }
    MailText.Add('From: your.email@gmail.com');
    MailText.Add('Subject: Team Announcement');
    MailText.Add('');
    MailText.Add('Hello team!');
    MailText.Add('This is an announcement for everyone.');

    { Add recipients }
    Recipients.Add('person1@example.com');
    Recipients.Add('person2@example.com');
    Recipients.Add('person3@example.com');

    Mail := TSMTPSend.Create;
    try
      Mail.TargetHost := 'smtp.gmail.com';
      Mail.TargetPort := '587';
      Mail.Username := 'your.email@gmail.com';
      Mail.Password := 'your.app.password';
      Mail.FullSSL := False;

      { Send to each recipient }
      for var Recipient in Recipients do
      begin
        WriteLn('Sending to: ', Recipient);
        Mail.SendToRaw('your.email@gmail.com', Recipient, MailText);
      end;

      WriteLn('All emails sent!');

    finally
      Mail.Free;
    end;

  finally
    MailText.Free;
    Recipients.Free;
  end;

  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Error Handling

Check for problems when sending:

```pascal
var
  Mail: TSMTPSend;
begin
  Mail := TSMTPSend.Create;
  try
    Mail.TargetHost := 'smtp.gmail.com';
    Mail.TargetPort := '587';
    Mail.Username := 'your.email@gmail.com';
    Mail.Password := 'your.app.password';
    Mail.FullSSL := False;

    if Mail.SendToRaw(FromAddr, ToAddr, MailText) then
    begin
      WriteLn('Success!');
    end
    else
    begin
      WriteLn('Failed to send email');
      WriteLn('Status code: ', Mail.EnhancedStatusCode);
      WriteLn('Result: ', Mail.ResultString);
    end;

  finally
    Mail.Free;
  end;
end;
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