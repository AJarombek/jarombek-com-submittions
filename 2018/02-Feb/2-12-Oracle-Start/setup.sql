-- Author: Andrew Jarombek
-- Date: 2/12/2018
-- Setup Users and permissions for Oracle 12c database

-- Look at all the tables in the database
SELECT * FROM SYS.ALL_TABLES;

-- We need to change the session in SQLPlus to the pluggable database CAPYBARA1 in order
-- to create our user
-- alter session set container=CAPYBARA1
-- show pdbs

CREATE USER ANDYUSR IDENTIFIED BY orac1et3st DEFAULT TABLESPACE ANDY;
GRANT ALL PRIVILEGES TO ANDYUSR;

-- Create a tablespace that I will use for my examples
CREATE TABLESPACE ANDY;

-- 'C##' is the common user prefix.  Use this prefix to create a user that isn't local to a single pluggable database
CREATE USER C##ANDYUSR IDENTIFIED BY orac1et3st DEFAULT TABLESPACE ANDY;
GRANT ALL PRIVILEGES TO C##ANDYUSR;

SELECT * FROM ALL_USERS WHERE USERNAME LIKE 'C##%';

-- Look at all the pluggable databases
SELECT * FROM DBA_PDBS;
