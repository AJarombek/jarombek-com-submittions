-- Author: Andrew Jarombek
-- Date: 2/28/2018
-- Query the database

-- Select books written in the month of August 2014
SELECT * FROM books WHERE released
BETWEEN to_date('2014-08-01', 'YYYY-mm-dd')
AND to_date('2014-08-31', 'YYYY-mm-dd');

-- Look for a book that contains the word 'Java'
SELECT * FROM books WHERE title LIKE '%Java%';

/*
ISBN           TITLE              RELEASED   STARTED   FINISHED  EDITION TIME_READING
-------------- ------------------ ---------- --------- --------- ------- ------------
9781617291999  Java 8 in Action   01-AUG-14  10-OCT-17 08-FEB-18         121
 */

-- Join across the many-to-many relationship to get the book title and corresponding language
SELECT b.isbn, b.title, bl.name, l.created
FROM books b
INNER JOIN book_languages bl ON b.isbn = bl.isbn
INNER JOIN languages l ON bl.name = l.name
ORDER BY b.title;

/*
           ISBN TITLE                      NAME       CREATED
--------------- -------------------------- ---------- --------------
  9781491901946 AngularJS: Up and Running  JavaScript 04-DEC-95
  9781617291999 Java 8 in Action           Java       23-MAY-95
  9780071799355 Oracle Database 12c SQL    SQL        01-JAN-74
  9780071799355 Oracle Database 12c SQL    PL/SQL     01-JAN-92
  9781617291203 Spring In Action           Java       23-MAY-95
 */

-- nvl2() checks if a value is null.  If it isn't null, the second parameter is returned.
-- Otherwise the third param is returned.
SELECT title, nvl2(time_reading, 'Finished Reading', 'Not Completed') still_reading,
  CASE WHEN time_reading > 100 THEN 'Long Read'
    WHEN time_reading > 25 THEN 'Moderate Read'
    ELSE 'Short Read' END duration
FROM books;

/*
TITLE                       STILL_READING     DURATION
--------------------------- ----------------- ------------
Java 8 in Action            Finished Reading  Long Read
Oracle Database 12c SQL     Finished Reading  Moderate Read
AngularJS: Up and Running   Not Completed     Short Read
Jump Start: Bootstrap       Finished Reading  Short Read
Spring In Action            Finished Reading  Short Read
 */

-- Perform a hierarchical search on languages.  Use lpad() to help visualize the hierarchy
SELECT LEVEL, lpad(' ', 2 * LEVEL -1) || name AS language
FROM language_hierarchy
START WITH name = 'C'
CONNECT BY PRIOR name = influnced_by;

/*
    LEVEL LANGUAGE
--------- -----------------------
        1 C
        2   Java
        3     JavaScript
        4       JSON
        4       TypeScript
        3     PHP
        3     Python
        4       JavaScript
        5         JSON
        5         TypeScript
        4       Swift
        3     TypeScript
        2   JavaScript
        3     JSON
        3     TypeScript
        2   PHP
        2   Python
        3     JavaScript
        4       JSON
        4       TypeScript
        3     Swift
 */

-- You can also reverse the order and traverse the relationship upward by switching the columns in the CONNECT BY PRIOR
-- statement.  You also don't have to start with a root node in the relationship.
SELECT LEVEL, lpad(' ', 2 * LEVEL -1) || name AS language
FROM language_hierarchy
START WITH name = 'Java'
CONNECT BY PRIOR influnced_by = name;

/*
    LEVEL LANGUAGE
--------- -----------------------
        1 Java
        2   C
 */

-- Display the total days spent reading on each language.
-- Also use the ROLLUP clause to display the total days spend overall.
-- In this scenario you could also use the CUBE clause to the same effect.
SELECT l.name, sum(b.time_reading) FROM books b
  INNER JOIN book_languages l ON b.isbn = l.isbn
GROUP BY ROLLUP(l.name);

/*
NAME          SUM(B.TIME_READING)
------------- -------------------
Java          134
JavaScript
PL/SQL        26
SQL           26
              186
 */

-- You can use the grouping() function along with ROLLUP or CUBE.  It takes in a column adn returns 1 if the column
-- value is null and 0 if it exists.  We can use this to create a nice label for our total days spent row.
SELECT
  CASE grouping(l.name)
       WHEN 1 THEN '*Total Days'
       ELSE l.name
       END AS name,
  sum(b.time_reading) AS total_time_reading
  FROM books b
  INNER JOIN book_languages l ON b.isbn = l.isbn
GROUP BY ROLLUP(l.name);

/*
NAME          TOTAL_TIME_READING
------------- -------------------
Java          134
JavaScript
PL/SQL        26
SQL           26
*Total Days   186
 */