# Connecting to APIs with Authentication

Many web services require authentication (login credentials) before letting you access their data. Let's learn how to handle different types of authentication in Free Pascal.

## Types of API Authentication

### 1. API Key Authentication

Some APIs give you a key to include with every request:

```pascal linenums="1"
program APIKeyAuth;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  opensslsockets,
  fphttpclient,
  fpjson,
  jsonparser;

var
  client: TFPHTTPClient;
  url: string;
  apiKey: string;
  response: string;

begin
  { Get your API key from the service }
  apiKey := 'your-api-key-here';

  client := TFPHTTPClient.Create(nil);
  try
    { Add the API key as a header }
    client.AddHeader('X-API-Key', apiKey);

    { Make the request }
    url := 'https://api.example.com/data';
    response := client.SimpleGet(url);

    WriteLn('Response: ', response);

  finally
    client.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

### 2. Bearer Token Authentication

Bearer tokens are common in modern APIs:

```pascal linenums="1"
program BearerTokenAuth;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  opensslsockets,
  fphttpclient,
  fpjson,
  jsonparser;

var
  client: TFPHTTPClient;
  url: string;
  token: string;
  response: string;

begin
  { Get your token from the API }
  token := 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...';

  client := TFPHTTPClient.Create(nil);
  try
    { Add the bearer token in the Authorization header }
    client.AddHeader('Authorization', 'Bearer ' + token);
    client.AddHeader('Content-Type', 'application/json');

    { Make the request }
    url := 'https://api.example.com/user/profile';
    response := client.SimpleGet(url);

    WriteLn('Response: ', response);

  finally
    client.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

### 3. Basic Authentication

Username and password sent as base64:

```pascal linenums="1"
program BasicAuth;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  opensslsockets,
  fphttpclient,
  Base64;

var
  client: TFPHTTPClient;
  url: string;
  username, password: string;
  credentials: string;
  response: string;

begin
  username := 'your_username';
  password := 'your_password';

  client := TFPHTTPClient.Create(nil);
  try
    { Combine username and password }
    credentials := username + ':' + password;

    { Encode as base64 }
    credentials := EncodeStringBase64(credentials);

    { Add authorization header }
    client.AddHeader('Authorization', 'Basic ' + credentials);

    { Make the request }
    url := 'https://api.example.com/data';
    response := client.SimpleGet(url);

    WriteLn('Response: ', response);

  finally
    client.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## OAuth 2.0 (Simplified)

OAuth is used by big services like Google and GitHub. Here's a basic approach:

```pascal linenums="1"
program OAuth2Example;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  opensslsockets,
  fphttpclient,
  fpjson,
  jsonparser;

var
  client: TFPHTTPClient;
  requestBody: TJSONObject;
  requestString: string;
  responseData: TJSONObject;
  accessToken: string;
  response: TStringStream;
  response2: TStringStream;

begin
  { OAuth 2.0 has several steps, here's a simplified example }

  { Step 1: Get an access token (after user authorization) }
  client := TFPHTTPClient.Create(nil);
  requestBody := TJSONObject.Create;
  responseData := TJSONObject.Create;
  response := TStringStream.Create('');
  response2 := TStringStream.Create('');

  try
    { Create the request for getting a token }
    requestBody.Strings['grant_type'] := 'authorization_code';
    requestBody.Strings['code'] := 'authorization-code-from-user';
    requestBody.Strings['client_id'] := 'your-client-id';
    requestBody.Strings['client_secret'] := 'your-client-secret';
    requestBody.Strings['redirect_uri'] := 'http://localhost:8080/callback';

    requestString := requestBody.AsJSON;

    client.AddHeader('Content-Type', 'application/json');

    { Get the token - use Post() with RequestBody }
    client.RequestBody := TRawByteStringStream.Create(requestString);
    client.Post('https://provider.example.com/oauth/token', response);

    responseData := TJSONObject(GetJSON(response.DataString));
    accessToken := responseData.Strings['access_token'];

    WriteLn('Got access token: ', accessToken);

    { Step 2: Use the access token to access protected resources }
    client.AddHeader('Authorization', 'Bearer ' + accessToken);

    client.Get('https://api.example.com/user/info', response2);
    WriteLn('User info: ', response2.DataString);

  finally
    client.Free;
    requestBody.Free;
    responseData.Free;
    response.Free;
    response2.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Handling Authentication Errors

When authentication fails, the API returns specific status codes:

```pascal linenums="1"
program HandleAuthErrors;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  opensslsockets,
  fphttpclient;

var
  client: TFPHTTPClient;
  response: string;
  statusCode: integer;

begin
  client := TFPHTTPClient.Create(nil);
  try
    client.AddHeader('Authorization', 'Bearer invalid-token');

    try
      response := client.SimpleGet('https://api.example.com/data');
      statusCode := client.ResponseStatusCode;

      if statusCode = 200 then
        WriteLn('Success: ', response)
      else if statusCode = 401 then
        WriteLn('Unauthorized - check your credentials or token')
      else if statusCode = 403 then
        WriteLn('Forbidden - you don''t have permission')
      else if statusCode = 404 then
        WriteLn('Not found')
      else
        WriteLn('Error code: ', statusCode);

    except
      on E: Exception do
        WriteLn('Connection error: ', E.Message);
    end;

  finally
    client.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Storing Credentials Safely

⚠️ **Never hardcode credentials in your code!**

Instead, use environment variables:

```pascal
var
  apiKey: string;
  token: string;
begin
  { Get from environment variables }
  apiKey := GetEnvironmentVariable('API_KEY');
  token := GetEnvironmentVariable('AUTH_TOKEN');

  client.AddHeader('X-API-Key', apiKey);
  client.AddHeader('Authorization', 'Bearer ' + token);
end;
```

Or use a configuration file:

```pascal
var
  configFile: TStringList;
  apiKey: string;
begin
  configFile := TStringList.Create;
  try
    configFile.LoadFromFile('config.ini');
    apiKey := configFile.Values['api_key'];
  finally
    configFile.Free;
  end;
end;
```

## Common Authentication Issues

| Problem | Solution |
|---------|----------|
| 401 Unauthorized | Check your credentials/token |
| 403 Forbidden | Your credentials work but you don't have permission |
| Token expired | Get a new token from the API |
| Invalid header format | Make sure "Bearer " or "Basic " prefixes are correct |
| CORS error | This happens in web browsers, not in desktop apps |

## HTTP Status Codes for Authentication

- **200** - Success
- **400** - Bad request (wrong format)
- **401** - Unauthorized (bad credentials)
- **403** - Forbidden (no permission)
- **429** - Too many requests (rate limited)
- **500** - Server error

## Best Practices

1. **Store credentials in environment variables** or config files, not in code
2. **Use HTTPS** (all examples above use https://)
3. **Check status codes** before processing responses
4. **Handle token expiration** - refresh tokens when needed
5. **Log errors** but don't log sensitive information
6. **Rate limiting** - respect API limits (usually 429 status code)

## Testing Authentication

Use a tool to test your authentication setup before using it in code:

- **Postman** - GUI tool for testing APIs
- **curl** - Command line tool
- **Thunder Client** - VS Code extension

## Next Steps

- Get an API key from a service and test authentication
- Implement error handling for failed authentication
- Create a function that handles token refresh automatically
- Test with a real API service
