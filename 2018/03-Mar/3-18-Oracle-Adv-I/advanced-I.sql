-- Author: Andrew Jarombek
-- Date: 3/2/2018
-- Query the database with advanced queries (Part I)

-- Our first analytic function ranks languages and the time spent reading about them
-- The difference between RANK() and DENSE_RANK() is that RANK() leaves a gap in the ranking sequence in the case of
-- a tie.  DENSE_RANK() does not.  Also, by default rankings put null values first, so to change this I used the
-- NULLS LAST clause
SELECT
  l.name,
  SUM(b.time_reading),
  RANK() OVER (ORDER BY SUM(b.time_reading) DESC NULLS LAST) AS rank,
  DENSE_RANK() OVER (ORDER BY SUM(b.time_reading) DESC NULLS LAST) AS dense_rank
FROM books b
INNER JOIN book_languages l ON b.isbn = l.isbn
GROUP BY l.name
ORDER BY rank;

/*
NAME         SUM(B.TIME_READING) RANK      DENSE_RANK
------------ ------------------- --------- ---------------
Java         134                 1         1
PL/SQL       26                  2         2
SQL          26                  2         2
JavaScript                       4         3
 */

-- Using the PARTITION BY clause I can divide the ranks into subgroups.  In this example, each year gets its own
-- ranking sequence along with an all-time ranking
SELECT
  year,
  language,
  lines,
  RANK() OVER (PARTITION BY year ORDER BY lines DESC) AS yearly_rank,
  RANK() OVER (ORDER BY lines DESC) AS all_time_rank
FROM code_written
ORDER BY year, yearly_rank;

/*
YEAR    LANGUAGE       LINES           YEARLY_RANK     ALL_TIME_RANK
------- -------------- --------------- --------------- ---------------
2014	  Java	         4282	           1	             7
2015	  Java	         1585          	 1	             14
2015	  Python	       931	           2	             23
2015	  C	             630	           3	             27
2015	  XML	           42	             4	             40
2015	  JSON	         32	             5	             42
2016	  Java	         12962	         1	             1
2016	  PHP	           5433	           2	             6
2016	  XML	           2646	           3	             9
2016	  JavaScript	   2008	           4	             10
2016	  HTML	         1413	           5	             15
....    ....           ....            .               ..
 */

-- Perform a yearly and all-time percent rank.  Since the numbers returned from this function can have many decimal
-- places, I round all numbers to two decimal places.
SELECT
  year,
  language,
  lines,
  ROUND(PERCENT_RANK() OVER (PARTITION BY year ORDER BY lines DESC), 2) AS yearly_distribution,
  ROUND(PERCENT_RANK() OVER (ORDER BY lines DESC), 2) AS all_time_distribution
FROM code_written
ORDER BY year, yearly_distribution;

/*
YEAR     LANGUAGE     LINES           YEARLY_DISTRIBUTION ALL_TIME_DISTRIBUTION
-------- ------------ --------------- ------------------- ---------------------
2014	   Java	        4282	          0                  	0.14
2015	   Java	        1585	          0	                  0.3
2015	   Python	      931	            0.25	              0.5
2015	   C	          630	            0.5	                0.59
2015	   XML	        42	            0.75	              0.89
2015	   JSON	        32	            1	                  0.93
2016	   Java	        12962	          0	                  0
2016	   PHP	        5433	          0.1	                0.11
2016	   XML	        2646	          0.2	                0.18
2016	   JavaScript	  2008	          0.3	                0.2
2016	   HTML	        1413	          0.4	                0.32
....     ....         ....            ...                 ....

 */

-- Attempts to find a median value in a very ~~fancy~~ way that goes over my head described here:
-- http://psoug.org/definition/PERCENTILE_CONT.htm
SELECT
  language,
  PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY lines DESC) AS median
FROM code_written GROUP BY language;

/*
LANGUAGE              MEDIAN
--------------------- ----------
C                     325
CSS                   1233
HTML                  1413
JSON                  466
Java                  4282
JavaScript            2008
PHP                   3670
PL/SQL                203
Python                1026.5
SQL                   812
Sass                  303
Swift                 5414.5
TypeScript            991.5
XML                   1344
 */

-- Perform a cumulative sum over years
SELECT
  year,
  SUM(lines) AS total_lines,
  SUM(SUM(lines)) OVER (ORDER BY year ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) as cumulative_lines
FROM code_written
GROUP BY year;

/*
 YEAR     TOTAL_LINES CUMULATIVE_LINES
----- --------------- ----------------
 2014            4282             4282
 2015            3220             7502
 2016           29161            36663
 2017           47140            83803
 2018            5986            89789
 */

-- Perform a moving average by specifying that the current row and two preceding rows will be included in the
-- window calculation of AVG()
SELECT
  year,
  SUM(lines) AS total_lines,
  AVG(SUM(lines)) OVER (ORDER BY year ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) as cumulative_lines
FROM code_written
GROUP BY year;

-- Perform a centered average by specifying that the window consists of the current row, the previous row, and the
-- next row.
SELECT
  year,
  SUM(lines) AS total_lines,
  AVG(SUM(lines)) OVER (ORDER BY year ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) as cumulative_lines
FROM code_written
GROUP BY year;

-- Display last years total lines along with the percent change year-to-year
SELECT
  year,
  SUM(lines) AS total_lines,
  FIRST_VALUE(SUM(lines)) OVER (ORDER BY year ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS last_year_total,
  ROUND(SUM(lines)/FIRST_VALUE(SUM(lines)) OVER (ORDER BY year ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) * 100, 2)
    || '%' AS percent_change
FROM code_written
GROUP BY year;

/*
  YEAR     TOTAL_LINES LAST_YEAR_TOTAL PERCENT_CHANGE
------ --------------- --------------- ---------------
  2014            4282            4282 100%
  2015            3220            4282 75.2%
  2016           29161            3220 905.62%
  2017           47140           29161 161.65%
  2018            5986           47140 12.7%
 */

-- Display a list of all the languages used in a year along with the total number of lines
-- The list of languages is ordered from most used to least used
SELECT
  year,
  LISTAGG(language, ', ') WITHIN GROUP (ORDER BY year, lines desc) AS languages_used,
  SUM(lines) as total_lines
FROM code_written
GROUP BY year;

/*
YEAR  LANGUAGES_USED                                                                                TOTAL_LINES
----- --------------------------------------------------------------------------------------------- ------------
2014	Java	                                                                                        4282
2015	Java, Python, C, XML, JSON	                                                                  3220
2016	Java, PHP, XML, JavaScript, HTML, CSS, Python, SQL, JSON, C, PL/SQL	                          29161
2017	Java, Swift, JavaScript, XML, PHP, HTML, CSS, Python, JSON, PL/SQL, SQL, C, Sass, TypeScript	47140
2018	TypeScript, JavaScript, Java, SQL, Sass, HTML, Python, PL/SQL, JSON, Swift, CSS, C, PHP, XML	5986
 */