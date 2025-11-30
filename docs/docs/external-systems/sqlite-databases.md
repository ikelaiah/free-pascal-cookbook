# Working with SQLite Databases

SQLite is a simple database that's perfect for learning. You don't need a separate server—it's just a file on your computer. Free Pascal makes it easy to work with SQLite databases.

## What is CRUD?

CRUD stands for the four basic things you can do with data:

- **Create** - Add new records to the database
- **Read** - Get data from the database
- **Update** - Change existing records
- **Delete** - Remove records

## Setting Up SQLite

First, you need the SQLite library. Most systems already have it installed. In your Free Pascal code, you'll use the `sqlite3` unit.

## Simple Database Example: Student Records

Here's a complete program that creates a database, adds students, reads them, updates one, and deletes one.

```pascal linenums="1"
program StudentDatabase;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  sqlite3;

var
  DB: PSQLite3;
  Stmt: PSQLite3_Stmt;
  ErrMsg: PChar;
  rc: integer;
  id, age: integer;
  name: string;

begin
  { Open or create the database }
  rc := sqlite3_open('students.db', @DB);
  if rc <> SQLITE_OK then
  begin
    WriteLn('Error opening database');
    Exit;
  end;

  { Create the students table }
  sqlite3_exec(DB, 'CREATE TABLE IF NOT EXISTS students (' +
    'id INTEGER PRIMARY KEY,' +
    'name TEXT NOT NULL,' +
    'age INTEGER);', nil, nil, @ErrMsg);

  WriteLn('=== Adding students (CREATE) ===');
  sqlite3_exec(DB, 'INSERT INTO students (name, age) VALUES (''Alice'', 17);', nil, nil, @ErrMsg);
  sqlite3_exec(DB, 'INSERT INTO students (name, age) VALUES (''Bob'', 18);', nil, nil, @ErrMsg);
  sqlite3_exec(DB, 'INSERT INTO students (name, age) VALUES (''Charlie'', 16);', nil, nil, @ErrMsg);
  WriteLn('Added 3 students');

  WriteLn('');
  WriteLn('=== Reading all students (READ) ===');
  rc := sqlite3_prepare_v2(DB, 'SELECT id, name, age FROM students;', -1, @Stmt, nil);
  while sqlite3_step(Stmt) = SQLITE_ROW do
  begin
    id := sqlite3_column_int(Stmt, 0);
    name := string(PChar(sqlite3_column_text(Stmt, 1)));
    age := sqlite3_column_int(Stmt, 2);
    WriteLn('ID: ', id, ' | Name: ', name, ' | Age: ', age);
  end;
  sqlite3_finalize(Stmt);

  WriteLn('');
  WriteLn('=== Updating Bob''s age to 19 (UPDATE) ===');
  sqlite3_exec(DB, 'UPDATE students SET age = 19 WHERE name = ''Bob'';', nil, nil, @ErrMsg);
  WriteLn('Updated Bob');

  WriteLn('');
  WriteLn('=== Reading after update ===');
  rc := sqlite3_prepare_v2(DB, 'SELECT id, name, age FROM students WHERE name = ''Bob'';', -1, @Stmt, nil);
  while sqlite3_step(Stmt) = SQLITE_ROW do
  begin
    id := sqlite3_column_int(Stmt, 0);
    name := string(PChar(sqlite3_column_text(Stmt, 1)));
    age := sqlite3_column_int(Stmt, 2);
    WriteLn('ID: ', id, ' | Name: ', name, ' | Age: ', age);
  end;
  sqlite3_finalize(Stmt);

  WriteLn('');
  WriteLn('=== Deleting Charlie (DELETE) ===');
  sqlite3_exec(DB, 'DELETE FROM students WHERE name = ''Charlie'';', nil, nil, @ErrMsg);
  WriteLn('Deleted Charlie');

  WriteLn('');
  WriteLn('=== Reading final list ===');
  rc := sqlite3_prepare_v2(DB, 'SELECT id, name, age FROM students;', -1, @Stmt, nil);
  while sqlite3_step(Stmt) = SQLITE_ROW do
  begin
    id := sqlite3_column_int(Stmt, 0);
    name := string(PChar(sqlite3_column_text(Stmt, 1)));
    age := sqlite3_column_int(Stmt, 2);
    WriteLn('ID: ', id, ' | Name: ', name, ' | Age: ', age);
  end;
  sqlite3_finalize(Stmt);

  { Close the database }
  sqlite3_close(DB);
  WriteLn('');
  WriteLn('Database closed. File saved as students.db');
  ReadLn;
end.
```

## How It Works

1. **Open Database** - `sqlite3_open('students.db', @DB)` opens the file (or creates it if it doesn't exist)

2. **Create Table** - The `CREATE TABLE` command sets up the structure. Each student has:
   - `id` - A unique number (automatically increases)
   - `name` - Text for the student's name
   - `age` - A number for age

3. **Create (INSERT)** - Add new records with `INSERT INTO students VALUES (...)`

4. **Read (SELECT)** - Get data with `SELECT` and loop through results with `sqlite3_step`

5. **Update** - Change existing records with `UPDATE ... SET ... WHERE`

6. **Delete** - Remove records with `DELETE FROM ... WHERE`

## Getting Column Values

When you read from the database, you get back different types:

```pascal
{ Get different data types from a result }
sqlite3_column_int(Stmt, 0)              { Get an integer }
sqlite3_column_text(Stmt, 1)             { Get text }
sqlite3_column_double(Stmt, 2)           { Get a decimal number }
```

## Common Mistakes to Avoid

- Remember to use `sqlite3_finalize(Stmt)` after reading results, so you don't waste memory
- Use quotes correctly: single quotes `'` inside SQL, double quotes `"` in Pascal strings
- Always close the database with `sqlite3_close(DB)` when done

## The File

The database is saved as `students.db` in the same folder as your program. You can open it with database viewer tools to see the data.

## Next Steps

- Try changing the table structure (add more columns like email or grade)
- Use `WHERE` conditions to find specific students
- Try using `ORDER BY` to sort results