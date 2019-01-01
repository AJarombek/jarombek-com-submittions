-- Demonstrate output parameters in PL/SQL
-- Author: Andrew Jarombek
-- Date: 12/31/2018

CREATE OR REPLACE PROCEDURE info(
  p_author OUT VARCHAR2,
  p_date OUT DATE
) AS
BEGIN
  p_author := 'Andrew Jarombek';
  p_date := sysdate;
END;
/

DECLARE
  v_author VARCHAR2(127);
  v_date DATE;
BEGIN
  info(v_author, v_date);
  DBMS_OUTPUT.PUT_LINE(v_author);
  DBMS_OUTPUT.PUT_LINE(v_date);
END;
