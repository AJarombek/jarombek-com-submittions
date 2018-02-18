-- Author: Andrew Jarombek
-- Date: 2/12/2018
-- Populate Tables for the Oracle 12c database

-- Start inserting into the database!

-- ORA-02266: unique/primary keys in table referenced by enabled foreign keys
-- TRUNCATE TABLE languages;
-- In order to avoid this error, you must specify the CASCADE keyword after the truncate statement
TRUNCATE TABLE languages CASCADE;

-- Although permitted to do so, do not insert into the auto generated identity column
INSERT INTO languages(
  name, created
) VALUES (
  'Java', '23-MAY-1995'
);

INSERT INTO languages(
  name, created
) VALUES (
  'SQL', '01-JAN-1974'
);

INSERT INTO languages(
  name, created
) VALUES (
  'PL/SQL', '01-JAN-1992'
);

INSERT INTO languages(
  name, created
) VALUES (
  'JavaScript', '04-DEC-1995'
);

SELECT * FROM languages;

TRUNCATE TABLE books CASCADE;

-- You are not allowed to insert into virtual columns.  So if you try to insert into time_reading, you get the error:
-- ORA-54013: INSERT operation disallowed on virtual columns
INSERT INTO books(
  isbn, title, released, started, finished, edition
) VALUES (
  9781617291999, 'Java 8 in Action', '01-AUG-2014', '10-OCT-2017', '08-FEB-2018', 1
);

INSERT INTO books(
  isbn, title, released, started, finished, edition
) VALUES (
  9780071799355, 'Oracle Database 12c SQL', '10-SEP-2013', '02-SEP-2017', '28-SEP-2017', 1
);

INSERT INTO books(
  isbn, title, released, started, finished, edition
) VALUES (
  9781491901946, 'AngularJS: Up and Running', '01-SEP-2014', '15-JUN-2017', NULL, 1
);

SELECT * FROM books;

TRUNCATE TABLE book_languages CASCADE;

-- Perform multiple insert into statements with the help of a PL/SQL procedure
DECLARE
  PROCEDURE bulk_insert(
    p_isbn IN INTEGER,
    p_name IN VARCHAR2
  ) AS
  BEGIN
    INSERT INTO book_languages(
      isbn, name
    ) VALUES (
      p_isbn, p_name
    );
  END;
BEGIN
  bulk_insert(p_isbn => 9781617291999, p_name => 'Java');
  bulk_insert(p_isbn => 9780071799355, p_name => 'SQL');
  bulk_insert(p_isbn => 9780071799355, p_name => 'PL/SQL');
  bulk_insert(p_isbn => 9781491901946, p_name => 'JavaScript');
END;

SELECT * FROM book_languages;