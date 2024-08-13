# Parsing JSON

Since many services use JSON for exchanging information, wouldn't it be nice to know how to do this in Free Pascal and Lazarus?

## How do I make a GET request?

!!! Note

    The snippet below is based on a fine tutorial by [Marcus Fernström](https://medium.com/@marcusfernstrm/freepascal-and-json-337c04cad489).

    Watch the detailed explanation by Marcus on YouTube; [Learn how to consume JSON data in Free Pascal](https://www.youtube.com/watch?v=Gy-OcEPgTHg)

Here is an example on making a GET request of JSON data and displaying it on console.

1. In `uses` add `opensslsockets` and `fphttpclient` for making the GET request. Line 11-12.
2. Set two variables. Line 16-17.
      - One for the URL. Here I use `https://dummyjson.com/users?limit=3`.
      - One for the response of the GET request.
3. Make the GET request by using `TFPHTTPClient.SimpleGet(url)`. Line 22.
4. Print result on console. Line 25.

```pascal linenums="1"  hl_lines="11-12 16 17 22 25"
program GetRequest;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  // For making web requests
  opensslsockets,
  fphttpclient;


var
  url: string = 'https://dummyjson.com/users?limit=3'; // endpoint to get JSON mock data
  rawJson: string; // a var to store raw JSON data

begin
  // Get the raw JSON data
  WriteLn('Contacting ', url, ' ...');
  rawJson := TFPHTTPClient.SimpleGet(url);

  // Display JSON on console
  WriteLn(rawJson);

  // Pause console
  WriteLn('Press enter key to exit...');
  ReadLn;
end.
```


## How do I parse JSON from `dummyjson.org`?

!!! Note

    Again, the snippet below is based on a fine tutorial by [Marcus Fernström](https://medium.com/@marcusfernstrm/freepascal-and-json-337c04cad489).

    Watch the detailed explanation by Marcus on YouTube; [Learn how to consume JSON data in Free Pascal](https://www.youtube.com/watch?v=Gy-OcEPgTHg)

Here is the breakdown of the example below.

1. Add necessary units for making the GET requests and parsing JSON data. Line 10-15.
2. Setup variables for receiving and processing the JSON data. Line 19-22.
     - A `string` to store raw JSON data.
     - A `TJSONArray` for storing an array of JSON objects.
     - A `TJSONEnum` for looping JSON arrays.
     - A `TJSONObject` for manipulating a JSON object.
3. Make the GET request and store the raw JSON response. Line 27.
4. Retrieve the array as TJSONArray. Line 33.
      - Convert the raw `JSON` data to `TJSONData`,
      - find data called "`users`" (a JSON array as per dummyjson's specs) and
      - Lastly, cast as `TJSONArray`.
5. Use the `TJSONEnum` var to loop through the JSON array. Line 36.
6. To use the JSON  object in the loop, cast the `TJSONEnum .value`, which is `TJSONData` to `TJSONObject`. Line 39.
7. Once you've obtained the JSONObject, use [`FindPath`](https://www.freepascal.org/docs-html/fcl/fpjson/tjsondata.findpath.html) to get value based on a key or a path.

```pascal linenums="1"
program ParseJSON;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  // For web requests
  opensslsockets,
  fphttpclient,
  // For parsing JSON data
  fpjson,
  jsonparser;

var
  url: string = 'https://dummyjson.com/users?limit=3'; // endpoint to get JSON mock data
  rawJson: string;       // a var to store the raw JSON data
  arrayJson: TJSONArray; // a var for storing an array of JSON objects
  enumJson: TJSONEnum;   // an enum type for looping JSON arrays
  objJson: TJSONObject;  // a var for manipulating a JSON object

begin
  // Get the raw JSON data
  WriteLn('Contacting ', url, ' ...');
  rawJson := TFPHTTPClient.SimpleGet(url);

  // Next, get the users array as TJSONArray;
  // 1. convert the raw JSON data to TJSONData,
  // 2. find data called "users" (a JSON array as per dummyjson's structure) and
  // 3. cast as TJSONArray
  arrayJson := TJSONArray(GetJSON(rawJson).FindPath('users'));

  // Loop using the TJSONEnum
  for enumJson in arrayJson do
  begin
    // Cast the enum value (TJSONData) to TJSONObject
    objJson := TJSONObject(enumJson.Value);

    // Output values based on keys/paths using `TJSONObject.FindPath('a_key')`
    WriteLn('id   : ', objJson.FindPath('id').AsString);
    WriteLn('name : ', objJson.FindPath('firstName').AsString, ' ', objJson.FindPath('lastName').AsString);
    WriteLn('phone: ', objJson.FindPath('phone').AsString);
    WriteLn('city : ', objJson.FindPath('address.city').AsString);
    WriteLn('state: ', objJson.FindPath('address.state').AsString);
    WriteLn('---');
  end;

  // Pause console
  WriteLn('Press enter key to exit...');
  ReadLn;
end.
```

### Why do we use `GetJSON(rawJson).FindPath('users')` to get the array?

 As per the specs of `https://dummyjson.com/users?limit=3`, the array of users is in `users`. Here is an response example from the `/users?limit=3` endpoint.

```json
{
  "users": [
    {
      "id": 1,
      "firstName": "Terry",
      "lastName": "Medhurst",
      "maidenName": "Smitham",
      "age": 50,
      "gender": "male",
      "email": "atuny0@sohu.com",
      "phone": "+63 791 675 8914",
      "username": "atuny0",
      "password": "9uQFF1Lh",
      "birthDate": "2000-12-25",
      "image": "https://robohash.org/Terry.png?set=set4",
      "bloodGroup": "A-",
      "height": 189,
      "weight": 75.4,
      "eyeColor": "Green",
      "hair": {
        "color": "Black",
        "type": "Strands"
      },
      "domain": "slashdot.org",
      "ip": "117.29.86.254",
      "address": {
        "address": "1745 T Street Southeast",
        "city": "Washington",
        "coordinates": {
          "lat": 38.867033,
          "lng": -76.979235
        },
        "postalCode": "20020",
        "state": "DC"
      },
      "macAddress": "13:69:BA:56:A3:74",
      "university": "Capitol University",
      "bank": {
        "cardExpire": "06/22",
        "cardNumber": "50380955204220685",
        "cardType": "maestro",
        "currency": "Peso",
        "iban": "NO17 0695 2754 967"
      },
      "company": {
        "address": {
          "address": "629 Debbie Drive",
          "city": "Nashville",
          "coordinates": {
            "lat": 36.208114,
            "lng": -86.58621199999999
          },
          "postalCode": "37076",
          "state": "TN"
        },
        "department": "Marketing",
        "name": "Blanda-O'Keefe",
        "title": "Help Desk Operator"
      },
      "ein": "20-9487066",
      "ssn": "661-64-2976",
      "userAgent": "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/534.24 (KHTML, like Gecko) Chrome/12.0.702.0 Safari/534.24",
      "crypto": {
        "coin": "Bitcoin",
        "wallet": "0xb9fc2fe63b2a6c003f1c324c3bfa53259162181a",
        "network": "Ethereum (ERC20)"
      }
    },
    {
      "id": 2,
      "firstName": "Sheldon",
      "lastName": "Quigley",
      "maidenName": "Cole",
      "age": 28,
      "gender": "male",
      "email": "hbingley1@plala.or.jp",
      "phone": "+7 813 117 7139",
      "username": "hbingley1",
      "password": "CQutx25i8r",
      "birthDate": "2003-08-02",
      "image": "https://robohash.org/Sheldon.png?set=set4",
      "bloodGroup": "O+",
      "height": 187,
      "weight": 74,
      "eyeColor": "Brown",
      "hair": {
        "color": "Blond",
        "type": "Curly"
      },
      "domain": "51.la",
      "ip": "253.240.20.181",
      "address": {
        "address": "6007 Applegate Lane",
        "city": "Louisville",
        "coordinates": {
          "lat": 38.1343013,
          "lng": -85.6498512
        },
        "postalCode": "40219",
        "state": "KY"
      },
      "macAddress": "13:F1:00:DA:A4:12",
      "university": "Stavropol State Technical University",
      "bank": {
        "cardExpire": "10/23",
        "cardNumber": "5355920631952404",
        "cardType": "mastercard",
        "currency": "Ruble",
        "iban": "MD63 L6YC 8YH4 QVQB XHIK MTML"
      },
      "company": {
        "address": {
          "address": "8821 West Myrtle Avenue",
          "city": "Glendale",
          "coordinates": {
            "lat": 33.5404296,
            "lng": -112.2488391
          },
          "postalCode": "85305",
          "state": "AZ"
        },
        "department": "Services",
        "name": "Aufderhar-Cronin",
        "title": "Senior Cost Accountant"
      },
      "ein": "52-5262907",
      "ssn": "447-08-9217",
      "userAgent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/534.30 (KHTML, like Gecko) Ubuntu/11.04 Chromium/12.0.742.112 Chrome/12.0.742.112 Safari/534.30",
      "crypto": {
        "coin": "Bitcoin",
        "wallet": "0xb9fc2fe63b2a6c003f1c324c3bfa53259162181a",
        "network": "Ethereum (ERC20)"
      }
    },
    {
      "id": 3,
      "firstName": "Terrill",
      "lastName": "Hills",
      "maidenName": "Hoeger",
      "age": 38,
      "gender": "male",
      "email": "rshawe2@51.la",
      "phone": "+63 739 292 7942",
      "username": "rshawe2",
      "password": "OWsTbMUgFc",
      "birthDate": "1992-12-30",
      "image": "https://robohash.org/Terrill.png?set=set4",
      "bloodGroup": "A-",
      "height": 200,
      "weight": 105.3,
      "eyeColor": "Gray",
      "hair": {
        "color": "Blond",
        "type": "Very curly"
      },
      "domain": "earthlink.net",
      "ip": "205.226.160.3",
      "address": {
        "address": "560 Penstock Drive",
        "city": "Grass Valley",
        "coordinates": {
          "lat": 39.213076,
          "lng": -121.077583
        },
        "postalCode": "95945",
        "state": "CA"
      },
      "macAddress": "F2:88:58:64:F7:76",
      "university": "University of Cagayan Valley",
      "bank": {
        "cardExpire": "10/23",
        "cardNumber": "3586082982526703",
        "cardType": "jcb",
        "currency": "Peso",
        "iban": "AT24 1095 9625 1434 9703"
      },
      "company": {
        "address": {
          "address": "18 Densmore Drive",
          "city": "Essex",
          "coordinates": {
            "lat": 44.492953,
            "lng": -73.101883
          },
          "postalCode": "05452",
          "state": "VT"
        },
        "department": "Marketing",
        "name": "Lindgren LLC",
        "title": "Mechanical Systems Engineer"
      },
      "ein": "48-3951994",
      "ssn": "633-89-1926",
      "userAgent": "Mozilla/5.0 (Windows NT 6.2; Win64; x64; rv:21.0.0) Gecko/20121011 Firefox/21.0.0",
      "crypto": {
        "coin": "Bitcoin",
        "wallet": "0xb9fc4b4b855bc44eb30d5e36fd18f491f44a15b7",
        "network": "Ethereum (ERC20)"
      }
    }
  ],
  "total": 100,
  "skip": 0,
  "limit": 3
}
```