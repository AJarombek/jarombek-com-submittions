-- Author: Andrew Jarombek
-- Date: 3/2/2018
-- Query the database with advanced queries (Part II)

-- Trying to account for verbosity when showing the number of lines written.
-- Trying to get a better measure of work done in each language
SELECT year, language, lines_written
  FROM code_written
MODEL
PARTITION BY (year)
DIMENSION BY (language)
  MEASURES (lines lines_written) (
    lines_written['Java'] = ROUND(lines_written['Java'] * 0.8),
    lines_written['Python'] = ROUND(lines_written['Python'] * 1.25)
  )
ORDER BY lines_written DESC NULLS LAST;

/*
YEAR  LANGUAGE      LINES_WRITTEN
----- ------------- ---------------
2017  Swift         10726
2016  Java          10370
2017  Java          9690
2017  JavaScript    6663
2017  XML           5815
2016  PHP           5433
2017  PHP           3670
....  ...           ....
 */

-- Get the monthly line totals for each language.  If no value is present, return 0
SELECT year, language, total_lines
  FROM code_written
MODEL
PARTITION BY (language)
DIMENSION BY (year)
  MEASURES (lines total_lines) (
    total_lines[FOR year FROM 2014 TO 2018 INCREMENT 1] =
      CASE WHEN total_lines[CURRENTV()] IS PRESENT THEN
        ROUND(total_lines[CURRENTV()] / 12, 0)
      ELSE
        0
      END
  )
ORDER BY total_lines DESC;

/*
YEAR  LANGUAGE      TOTAL_LINES
----- ------------- --------------
2016  Java          1080
2017  Java          1009
2017  Swift         894
2017  JavaScript    555
2017  XML           485
2016  PHP           453
....  ...           ...
 */

-- You can shorten the above code by using PRESENTNNV() instead of a case statement
-- This function takes a column and if it existed prior to the MODEL clause and isn't null, the second argument is
-- returned.  Otherwise, the third argument is returned
SELECT year, language, total_lines
  FROM code_written
MODEL
PARTITION BY (language)
DIMENSION BY (year)
  MEASURES (lines total_lines) (
    total_lines[FOR year FROM 2014 TO 2018 INCREMENT 1] =
      PRESENTNNV(total_lines[CURRENTV()], ROUND(total_lines[CURRENTV()] / 12, 0), 0)
  )
ORDER BY total_lines DESC;

-- You can shorten the query even further by using the IGNORE NAV clause with th MODEL.  This specifies that any null
-- values should return a default value.  For numbers that default is 0, which is the behavior we wanted
SELECT year, language, total_lines
  FROM code_written
MODEL IGNORE NAV
PARTITION BY (language)
DIMENSION BY (year)
  MEASURES (lines total_lines) (
    total_lines[FOR year FROM 2014 TO 2018 INCREMENT 1] = ROUND(total_lines[CURRENTV()] / 12, 0)
  )
ORDER BY total_lines DESC;

-- By default when using the MODEL clause if a row doesn't exist for an array entry, a new row is created.  We can change
-- that behavior so that no new row is created by using the RULES UPDATE clause.  Now no new rows are created that were
-- not in the base table
SELECT year, language, total_lines
  FROM code_written
MODEL
PARTITION BY (language)
DIMENSION BY (year)
  MEASURES (lines total_lines)
  RULES UPDATE (
    total_lines[FOR year FROM 2014 TO 2018 INCREMENT 1] = ROUND(total_lines[CURRENTV()] / 12, 0)
  )
ORDER BY total_lines DESC;

-- PIVOT rotates columns into rows.  Now each row is a language and the columns are each year.
SELECT * FROM (
  SELECT
    year,
    language,
    lines
  FROM code_written
)
PIVOT (
  SUM(lines) FOR year IN (2014, 2015, 2016, 2017, 2018)
);

/*
LANGUAGE       2014   2015   2016   2017   2018
-------------- ------ ------ ------ ------ ------
JavaScript		               2008	  6663	 1120
XML		                42	   2646	  5815	 0
PHP			                     5433	  3670	 0
Sass				                        163	   443
TypeScript				                  133	   1850
PL/SQL			                 203	  844	   121
SQL			                     942	  812	   585
C		                  630	   379	  271	   0
Java	         4282	  1585	 12962	12113	 979
Python		            931	   1122	  1288	 242
HTML			                   1413	  1969	 397
CSS			                     1233	  1654	 34
JSON		              32	   820	  1019	 112
Swift				                        10726	 103
 */

-- By using the ROLLUP clause in the inner query, we can also display the total number of lines for each language
DROP TABLE code_written_pivot;

CREATE TABLE code_written_pivot AS
SELECT * FROM (
  SELECT
    CASE GROUPING(year)
    WHEN 1
      THEN 'Total'
    ELSE TO_CHAR(year)
    END AS year,
    language,
    SUM(lines) AS lines
  FROM code_written
  GROUP BY ROLLUP (language, year)
)
PIVOT (
  SUM(lines)
FOR year IN (2014, 2015, 2016, 2017, 2018, 'Total' AS Total)
)
ORDER BY "2018" DESC NULLS LAST;

/*
LANGUAGE       2014   2015   2016   2017   2018   TOTAL
-------------- ------ ------ ------ ------ ------ ------
TypeScript				                  133	   1850   1983
JavaScript		               2008	  6663	 1120   9791
Java	         4282	  1585	 12962	12113	 979    31921
SQL			                     942	  812	   585    2339
Sass				                        163	   443    606
HTML			                   1413	  1969	 397    3779
Python		            931	   1122	  1288	 242    3583
PL/SQL			                 203	  844	   121    1168
JSON		              32	   820	  1019	 112    1983
Swift				                        10726	 103    10829
CSS			                     1233	  1654	 34     2921
C		                  630	   379	  271	   0      1280
XML		                42	   2646	  5815	 0      8503
PHP			                     5433	  3670	 0      9103
                                                  89789
 */

-- UNPIVOT does the opposite of PIVOT.  Although the table is not exactly the same as the original, it contains the
-- same data.  If column names start with a number, you must use double quotes to access them
SELECT * FROM code_written_pivot
UNPIVOT (
  lines FOR year IN (Total, "2018", "2017", "2016", "2015", "2014")
);

/*
LANGUAGE       YEAR            LINES
-------------- ----- ---------------
TypeScript     TOTAL            1983
TypeScript     2018             1850
TypeScript     2017              133
JavaScript     TOTAL            9791
JavaScript     2018             1120
JavaScript     2017             6663
JavaScript     2016             2008
Java           TOTAL           31921
Java           2018              979
Java           2017            12113
Java           2016            12962
Java           2015             1585
Java           2014             4282
*/

/* Other Random Queries */

-- Fetch the first row of the code_written table with the newest year.  Since we specify WITH TIES, all other rows with
-- the same year will also be returned
SELECT * FROM code_written
ORDER BY year DESC
FETCH FIRST 1 ROWS WITH TIES;

-- Average lines written in each language per year
SELECT
  language,
  ROUND(avg(lines), 2)
FROM code_written GROUP BY language;

-- Sort the table by a column starting with a number (use double quotes)
SELECT * FROM code_written_pivot
ORDER BY "2018" DESC NULLS LAST;