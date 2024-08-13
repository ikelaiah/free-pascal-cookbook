# Webserver

## A simple REST APIs returning JSON and HTML contents

Have a look at the example below, based on a tutorial by [Marcus Fernström](https://medium.com/@marcusfernstrm/create-rest-apis-with-freepascal-441e4aa447b7).

The program creates a basic REST APIs with three routes.

  - `/api/timestamp` to give current timestamp in JSON.
  - `/api/greet/:name` to return a specified `name` in JSON.
  - `/404` for handling everything else.

Here is the breakdown.

1. In the `unit`, add `fphttpapp`, `HTTPDefs`, `httproute`, `fpjson` and `jsonparser`. Line 16-20.
2. Setup port no and enable multi-threading to process incoming http requests. Line 77-78.
3. Register routes and set `/404` as the default route. Line 81-83.
4. Define procedures to handle the routes. Line 23-67.
5. Initialise and run. Line 86-88. 

```pascal linenums="1" hl_lines="16-20 77-78 81-83 86-88"
program SimpleApiWebserver;

// References:
//  - https://medium.com/@marcusfernstrm/create-rest-apis-with-freepascal-441e4aa447b7
//  - https://www.youtube.com/watch?v=9N0cxI1Hp0U
//  - https://wiki.lazarus.freepascal.org/fpWeb_Tutorial#webserver-example

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  fphttpapp,
  HTTPDefs,
  httproute,
  fpjson,
  jsonparser;

  // Endpoint for getting the current timestamp
  procedure TimestampEndPoint(aRequest: TRequest; aResponse: TResponse);
  var
    json: TJSONObject;
  begin
    json := TJSONObject.Create;
    try
      json.Strings['timestamp'] := FormatDateTime('yyyy-mm-dd hh:nn:ss.z', Now);
      aResponse.Content := json.AsJSON;
      aResponse.Code := 200;
      aResponse.ContentType := 'application/json';
      aResponse.ContentLength := Length(aResponse.Content);
      aResponse.SendContent;
    finally
      json.Free;
    end;

  end;

  // An endpoint accepting a variable
  procedure GreetEndpoint(aRequest: TRequest; aResponse: TResponse);
  var
    json: TJSONObject;
  begin
    json := TJSONObject.Create;
    try
      json.Strings['message'] := 'Hello, ' + aRequest.RouteParams['name'];
      aResponse.Content := json.AsJSON;
      aResponse.Code := 200;
      aResponse.ContentType := 'application/json';
      aResponse.ContentLength := Length(aResponse.Content);
      aResponse.SendContent;
    finally
      json.Free;
    end;
  end;

  // An endpoint to handle unknown requests
  procedure ErrorEndPoint(aRequest: TRequest; aResponse: TResponse);
  begin
    aResponse.Content := '<h1>404!</h1><h3>How did you get here?</h3>';
    aResponse.Code := 404;
    aResponse.ContentType := 'text/html';
    aResponse.ContentLength := Length(aResponse.Content);
    aResponse.SendContent;
  end;


const
  port: integer = 8080;
  isThreaded: boolean = True;

begin

  // Set port no & enable multi-threading to handle http requests
  Application.Port := port;
  Application.Threaded := isThreaded;

  // Setup routes
  HTTPRouter.RegisterRoute('/api/timestamp', rmGet, @TimestampEndPoint);
  HTTPRouter.RegisterRoute('/api/greet/:name', rmGet, @GreetEndpoint);
  HTTPRouter.RegisterRoute('/404', rmGet, @ErrorEndPoint, True); // Set as the default endpoint

  // Initialise and run, with a message
  Application.Initialize;
  WriteLn('== Server is running on port ', port, ' ===');
  Application.Run;

end.
```

**How to use**

- Run the program.
  - To get the current timestamp, put into your browser: `http://127.0.0.1:8080/api/timestamp`.
  - To get a greeting in json, put into your browser: `http://127.0.0.1:8080/api/greet/Jonathan`.
  - Non-matching endpoints will get you to: `http://127.0.0.1:8080/404`.


**References**

- [https://medium.com/@marcusfernstrm/create-rest-apis-with-freepascal-441e4aa447b7](https://medium.com/@marcusfernstrm/create-rest-apis-with-freepascal-441e4aa447b7)
- [https://www.youtube.com/watch?v=9N0cxI1Hp0U](https://www.youtube.com/watch?v=9N0cxI1Hp0U)
- [https://wiki.lazarus.freepascal.org/fpWeb_Tutorial#webserver-example](https://wiki.lazarus.freepascal.org/fpWeb_Tutorial#webserver-example)