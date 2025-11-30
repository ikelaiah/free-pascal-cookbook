# Working with XML

XML is another format for storing and sharing data, like JSON. It's been around longer and is used a lot in business software. If you understand JSON, XML is not too different.

## What is XML?

XML looks like this:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<students>
  <student>
    <id>1</id>
    <name>Alice</name>
    <age>17</age>
    <grade>A</grade>
  </student>
  <student>
    <id>2</id>
    <name>Bob</name>
    <age>18</age>
    <grade>B</grade>
  </student>
</students>
```

Key differences from JSON:

- XML uses **tags** (like `<name>`) instead of quotes
- XML is **more wordy** but easier for humans to read
- Many older systems still use XML

## Parsing XML in Free Pascal

Here's how to read and parse XML data:

```pascal linenums="1"
program ParseXML;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  DOM,
  XMLRead,
  XMLWrite;

var
  Doc: TXMLDocument;
  RootNode, StudentNode, ChildNode: TDOMElement;
  NodeList: TDOMNodeList;
  i: integer;
  id, name, age, grade: string;

begin
  { Load XML file }
  ReadXMLFile(Doc, 'students.xml');

  try
    { Get the root element (students) }
    RootNode := Doc.DocumentElement;
    WriteLn('Root element: ', RootNode.NodeName);

    { Get all student nodes }
    NodeList := RootNode.GetElementsByTagName('student');

    WriteLn('Found ', NodeList.Count, ' students:');
    WriteLn('');

    { Loop through each student }
    for i := 0 to NodeList.Count - 1 do
    begin
      StudentNode := TDOMElement(NodeList[i]);

      { Get each piece of information }
      id := StudentNode.FindNode('id').TextContent;
      name := StudentNode.FindNode('name').TextContent;
      age := StudentNode.FindNode('age').TextContent;
      grade := StudentNode.FindNode('grade').TextContent;

      WriteLn('ID: ', id);
      WriteLn('Name: ', name);
      WriteLn('Age: ', age);
      WriteLn('Grade: ', grade);
      WriteLn('---');
    end;

  finally
    Doc.Free;
  end;

  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Creating XML Files

You can also create XML files programmatically:

```pascal linenums="1"
program CreateXML;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  DOM,
  XMLWrite;

var
  Doc: TXMLDocument;
  RootNode, StudentNode, ChildNode: TDOMElement;

begin
  { Create a new XML document }
  Doc := TXMLDocument.Create;

  try
    { Create the root element }
    RootNode := Doc.CreateElement('students');
    Doc.AppendChild(RootNode);

    { Add first student }
    StudentNode := Doc.CreateElement('student');
    RootNode.AppendChild(StudentNode);

    ChildNode := Doc.CreateElement('id');
    ChildNode.TextContent := '1';
    StudentNode.AppendChild(ChildNode);

    ChildNode := Doc.CreateElement('name');
    ChildNode.TextContent := 'Alice';
    StudentNode.AppendChild(ChildNode);

    ChildNode := Doc.CreateElement('age');
    ChildNode.TextContent := '17';
    StudentNode.AppendChild(ChildNode);

    { Add second student }
    StudentNode := Doc.CreateElement('student');
    RootNode.AppendChild(StudentNode);

    ChildNode := Doc.CreateElement('id');
    ChildNode.TextContent := '2';
    StudentNode.AppendChild(ChildNode);

    ChildNode := Doc.CreateElement('name');
    ChildNode.TextContent := 'Bob';
    StudentNode.AppendChild(ChildNode);

    ChildNode := Doc.CreateElement('age');
    ChildNode.TextContent := '18';
    StudentNode.AppendChild(ChildNode);

    { Save to file }
    WriteXMLFile(Doc, 'students.xml');
    WriteLn('XML file created: students.xml');

  finally
    Doc.Free;
  end;

  WriteLn('Press enter to exit...');
  ReadLn;
end.
```

## Attributes vs Child Elements

XML can store information two ways:

**As attributes:**
```xml
<student id="1" name="Alice" age="17" />
```

**As child elements:**
```xml
<student>
  <id>1</id>
  <name>Alice</name>
  <age>17</age>
</student>
```

Both work the same way in Free Pascal:

```pascal
{ Reading attributes }
id := StudentNode.AttribStrings['id'];
name := StudentNode.AttribStrings['name'];

{ Reading child elements }
id := StudentNode.FindNode('id').TextContent;
name := StudentNode.FindNode('name').TextContent;
```

## Handling Missing Elements

When reading XML, sometimes an element might not exist. You should check first:

```pascal
var
  node: TDOMNode;
begin
  node := StudentNode.FindNode('grade');
  if node <> nil then
    WriteLn('Grade: ', node.TextContent)
  else
    WriteLn('No grade found');
end;
```

## Using XML in Real Projects

Many services provide XML data. For example:

- **Weather APIs** - Some weather services return XML
- **RSS feeds** - Blog updates come in XML format
- **Configuration files** - Enterprise apps often use XML config files
- **Office documents** - Word and Excel can save as XML

## When to Use XML vs JSON

- **Use JSON** if you're building new projects (it's simpler and smaller)
- **Use XML** if you're working with existing systems or if the data is very structured

## Common Units You'll Need

```pascal
uses
  Classes,          { For basic functionality }
  DOM,             { For XML documents and elements }
  XMLRead,         { For reading XML files }
  XMLWrite;        { For writing/saving XML files }
```

## Next Steps

- Try parsing a real XML file
- Create XML files from database records
- Fetch XML data from a web service and parse it