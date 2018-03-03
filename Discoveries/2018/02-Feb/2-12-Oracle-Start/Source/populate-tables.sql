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

DECLARE
  PROCEDURE language_bulk_insert(
    p_name IN languages.name%TYPE,
    p_created IN languages.created%TYPE
  ) AS
  BEGIN
    INSERT INTO languages(
      name, created
    ) VALUES (
      p_name, p_created
    );
  END;
BEGIN
  language_bulk_insert('HTML', '01-JAN-1993');
  language_bulk_insert('XML', '01-JAN-1996');
  language_bulk_insert('CSS', '17-DEC-1996');
  language_bulk_insert('Sass', '28-NOV-2006');
  language_bulk_insert('Swift', '02-JUN-2014');
  language_bulk_insert('PHP', '01-JAN-1995');
  language_bulk_insert('Python', '20-FEB-1991');
  language_bulk_insert('TypeScript', '01-OCT-2012');
  language_bulk_insert('JSON', '01-JAN-2002');
  language_bulk_insert('C', '01-JAN-1972');
END;

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

DECLARE
  PROCEDURE book_bulk_insert(
    p_isbn IN books.isbn%TYPE,
    p_title IN books.title%TYPE,
    p_released IN books.released%TYPE,
    p_started IN books.started%TYPE,
    p_finished IN books.finished%TYPE,
    p_edition IN books.edition%TYPE
  ) AS
  BEGIN
    INSERT INTO books(
      isbn, title, released, started, finished, edition
    ) VALUES (
      p_isbn, p_title, p_released, p_started, p_finished, p_edition
    );
  END;
BEGIN
  book_bulk_insert(9781457192296, 'Jump Start: Bootstrap', '25-MAY-2014', '02-JUL-2017', '05-JUL-2017', 1);
  book_bulk_insert(9781617291203, 'Spring In Action', '01-NOV-2014', '12-JUN-2016', '25-JUN-2016', 4);
END;

-- Change edition to null for all the books that are first edition
UPDATE books SET edition = NULL WHERE edition = 1;

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
  bulk_insert(p_isbn => 9781617291203, p_name => 'Java');
END;

SELECT * FROM book_languages;