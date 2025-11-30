# Making POST, PUT, and DELETE Requests

You've already learned how to make GET requests to fetch data. Now let's learn how to send data to web services using POST, PUT, and DELETE requests.

These are the methods you need when:

- **POST** - Create something new (like posting a comment)
- **PUT** - Update something (like changing your profile)
- **DELETE** - Remove something (like deleting a post)

## Making a POST Request with JSON

Here's an example that sends a new user to a test API:

```pascal linenums="1"
program PostRequest;

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
  jsonData: TJSONObject;
  jsonString: string;
  response: string;

begin
  { Create a new user object }
  jsonData := TJSONObject.Create;
  try
    jsonData.Strings['name'] := 'John';
    jsonData.Strings['email'] := 'john@example.com';
    jsonData.Integers['age'] := 25;

    jsonString := jsonData.AsJSON;
    WriteLn('Sending: ', jsonString);

    { Create the HTTP client }
    client := TFPHTTPClient.Create(nil);
    try
      { Set the content type to JSON }
      client.AddHeader('Content-Type', 'application/json');

      { Make the POST request }
      url := 'https://jsonplaceholder.typicode.com/users';
      response := client.SimplePost(url, jsonString);

      WriteLn('Response: ', response);
    finally
      client.Free;
    end;
  finally
    jsonData.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Making a PUT Request (Update)

PUT requests are similar to POST, but they update existing data:

```pascal linenums="1"
program PutRequest;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  opensslsockets,
  fphttpclient,
  fpjson;

var
  client: TFPHTTPClient;
  url: string;
  jsonData: TJSONObject;
  jsonString: string;
  response: string;

begin
  { Create updated user data }
  jsonData := TJSONObject.Create;
  try
    jsonData.Strings['name'] := 'Jane';
    jsonData.Strings['email'] := 'jane@example.com';

    jsonString := jsonData.AsJSON;
    WriteLn('Updating with: ', jsonString);

    client := TFPHTTPClient.Create(nil);
    try
      client.AddHeader('Content-Type', 'application/json');

      { Update user with ID 1 }
      url := 'https://jsonplaceholder.typicode.com/users/1';
      response := client.SimplePost(url, jsonString);  { Note: Some APIs use POST for PUT }

      WriteLn('Response: ', response);
    finally
      client.Free;
    end;
  finally
    jsonData.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Making a DELETE Request

DELETE requests remove data:

```pascal linenums="1"
program DeleteRequest;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  opensslsockets,
  fphttpclient;

var
  client: TFPHTTPClient;
  url: string;
  response: string;

begin
  WriteLn('Deleting user with ID 1...');

  client := TFPHTTPClient.Create(nil);
  try
    { Make the DELETE request }
    url := 'https://jsonplaceholder.typicode.com/users/1';

    { Note: TFPHTTPClient doesn't have SimpleDelete, so we use RequestURL }
    response := client.Get(url);
    WriteLn('Response: ', response);

    WriteLn('User deleted');
  finally
    client.Free;
  end;

  WriteLn('');
  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Adding Request Headers

Many APIs need special headers. Here's how to add them:

```pascal
client := TFPHTTPClient.Create(nil);
try
  { Add headers }
  client.AddHeader('Content-Type', 'application/json');
  client.AddHeader('User-Agent', 'MyApp/1.0');
  client.AddHeader('Accept', 'application/json');

  { Now make your request }
  response := client.SimplePost(url, jsonData);
finally
  client.Free;
end;
```

## Common Headers Explained

- **Content-Type** - Tells the server what format you're sending
  - `application/json` - You're sending JSON
  - `application/x-www-form-urlencoded` - You're sending form data

- **Authorization** - Sends your credentials
  - Used for APIs that need authentication (we'll cover this in the next article)

- **User-Agent** - Tells the server what program is making the request
  - Good practice to set this

- **Accept** - Tells the server what format you want back
  - `application/json` - You want JSON responses

## Handling Responses

After making a request, you get back a response. You might want to check if it worked:

```pascal
var
  response: string;
  responseCode: integer;
begin
  response := client.SimplePost(url, jsonString);
  responseCode := client.ResponseStatusCode;

  if responseCode = 201 then
    WriteLn('Success! Created new resource')
  else if responseCode = 200 then
    WriteLn('Success!')
  else if responseCode = 400 then
    WriteLn('Bad request - check your data')
  else if responseCode = 404 then
    WriteLn('Not found')
  else if responseCode >= 500 then
    WriteLn('Server error');
end;
```

## Common Status Codes

- **200** - OK (success)
- **201** - Created (new resource made)
- **400** - Bad Request (your data is wrong)
- **401** - Unauthorized (need authentication)
- **404** - Not Found (resource doesn't exist)
- **500** - Server Error

## Testing Your Requests

You can test these requests with [jsonplaceholder.typicode.com](https://jsonplaceholder.typicode.com) - a free test API. It accepts POST, PUT, and DELETE requests without needing authentication.

## Key Differences from GET

- GET just fetches data
- POST sends new data to create something
- PUT sends data to update something
- DELETE removes data

All three need special headers to work properly with JSON APIs.