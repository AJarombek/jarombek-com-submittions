-- Author: Andrew Jarombek
-- Date: 2/28/2018
-- Create a new table called language_hierarchy that contains hierarchical data

DROP TABLE language_hierarchy CASCADE CONSTRAINTS;

-- Both name and influenced_by fields come from the language tables name column
CREATE TABLE language_hierarchy(
  name VARCHAR2(63),
  influnced_by VARCHAR2(63),
  CONSTRAINT language_hierarchy_name_fk
    FOREIGN KEY (name) REFERENCES languages(name) ON DELETE CASCADE,
  CONSTRAINT language_hierarchy_inf_by_fk
    FOREIGN KEY (influnced_by) REFERENCES languages(name) ON DELETE CASCADE
);

TRUNCATE TABLE language_hierarchy CASCADE;

DECLARE
  PROCEDURE language_hierarchy_bulk_insert(
    p_name IN language_hierarchy.name%TYPE,
    p_influenced_by IN language_hierarchy.influnced_by%TYPE
  ) AS
  BEGIN
    INSERT INTO language_hierarchy(
      name, influnced_by
    ) VALUES (
      p_name, p_influenced_by
    );
  END;
BEGIN
  language_hierarchy_bulk_insert('Java', 'C');
  language_hierarchy_bulk_insert('C', NULL);
  language_hierarchy_bulk_insert('JavaScript', 'Java');
  language_hierarchy_bulk_insert('JavaScript', 'C');
  language_hierarchy_bulk_insert('JavaScript', 'Python');
  language_hierarchy_bulk_insert('SQL', NULL);
  language_hierarchy_bulk_insert('PL/SQL', 'SQL');
  language_hierarchy_bulk_insert('HTML', NULL);
  language_hierarchy_bulk_insert('XML', NULL);
  language_hierarchy_bulk_insert('CSS', NULL);
  language_hierarchy_bulk_insert('Sass', 'CSS');
  language_hierarchy_bulk_insert('Swift', 'Python');
  language_hierarchy_bulk_insert('PHP', 'C');
  language_hierarchy_bulk_insert('PHP', 'Java');
  language_hierarchy_bulk_insert('Python', 'C');
  language_hierarchy_bulk_insert('Python', 'Java');
  language_hierarchy_bulk_insert('TypeScript', 'Java');
  language_hierarchy_bulk_insert('TypeScript', 'JavaScript');
  language_hierarchy_bulk_insert('JSON', 'JavaScript');
END;

SELECT * FROM language_hierarchy;