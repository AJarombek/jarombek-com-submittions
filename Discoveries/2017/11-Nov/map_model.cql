// Author: Andrew Jarombek
// Date: 10/27/2017

// Create people that will be placed on the map
CREATE (aj:Person {name: 'Andy J.'}),
    (tc:Person {name: 'Tom C.'}),
    (js:Person {name: 'Joe S.'}),
    (bf:Person {name: 'Ben F.'})

// Delete previously created people
MATCH (p:Person) DELETE p

// Create people and specify where they live
MATCH (greenwich:Town {name: 'Greenwich'}),
    (sherman:Town {name: 'Sherman'}),
    (fairfield:Town {name: 'Fairfield'}),
    (easton:Town {name: 'Easton'})
CREATE (aj:Person {name: 'Andy J.'}),
    (aj)-[:LIVES_IN]->(greenwich),
    (tc:Person {name: 'Tom C.'}),
    (tc)-[:LIVES_IN]->(fairfield),
    (js:Person {name: 'Joe S.'}),
    (js)-[:LIVES_IN]->(easton),
    (bf:Person {name: 'Ben F.'}),
    (bf)-[:LIVES_IN]->(sherman)

// This creates the pizza places with three properties
// It also specifies where the pizza place is located - however there is a bug:
// New settlements are created for each pizza place LOCATED_IN relationship instead
// of attaching it to an existing location
CREATE (stan:Pizza {name: 'Stanziato’s Wood Fired Pizza', price: 2, rating: 4.5}),
        (stan)-[:LOCATED_IN]->(:Settlement {name: 'Danbury'}),
        (bw:Pizza {name: 'Brick + Wood', price: 2, rating: 4.5}),
        (bw)-[:LOCATED_IN]->(:Settlement {name: 'Fairfield'}),
        (pp:Pizza {name: 'Prime Pub - Danbury', price: 2, rating: 4.5}),
        (pp)-[:LOCATED_IN]->(:Settlement {name: 'Danbury'}),
        (cp:Pizza {name: 'Classic Pizza', price: 1, rating: 4.5}),
        (cp)-[:LOCATED_IN]->(:Settlement {name: 'Norwalk'}),
        (s:Pizza {name: 'Savor', price: 2, rating: 4.5}),
        (s)-[:LOCATED_IN]->(:Settlement {name: 'Norwalk'}),
        (spr:Pizza {name: 'Fire Engine Pizza Company', price: 2, rating: 4}),
        (spr)-[:LOCATED_IN]->(:Settlement {name: 'Bridgeport'}),
        (mpr:Pizza {name: 'Michael’s Pizzeria Restaurant', price: 2, rating: 4.5}),
        (mpr)-[:LOCATED_IN]->(:Settlement {name: 'Stamford'}),
        (piep:Pizza {name: 'Pieology Pizzeria', price: 2, rating: 5}),
        (piep)-[:LOCATED_IN]->(:Settlement {name: 'Stamford'}),
        (rompb:Pizza {name: 'Romanacci Pizza Bar', price: 2, rating: 4}),
        (rompb)-[:LOCATED_IN]->(:Settlement {name: 'Trumbull'}),
        (rom:Pizza {name: 'Romanacci', price: 2, rating: 4}),
        (rom)-[:LOCATED_IN]->(:Settlement {name: 'Norwalk'})

// This undos the previous mistake
MATCH (s:Settlement) 
WITH s.name as name, collect(s) AS instances
WHERE size(instances) > 1
UNWIND tail(instances) as dups
MATCH (dups)-[r:LOCATED_IN]-(pizza)
DELETE r, dups, pizza

// This code deletes a single person from the graph using their internal id
MATCH (p:Person) WHERE id(p) = 62 DETACH DELETE p

// Fix to the previous query so we dont duplicate nodes
CREATE (p1:Pizza {name: 'Stanziato’s Wood Fired Pizza', price: 2, rating: 4.5}),
        (p2:Pizza {name: 'Brick + Wood', price: 2, rating: 4.5}),
        (p3:Pizza {name: 'Prime Pub - Danbury', price: 2, rating: 4.5}),
        (p4:Pizza {name: 'Classic Pizza', price: 1, rating: 4.5}),
        (p5:Pizza {name: 'Savor', price: 2, rating: 4.5}),
        (p6:Pizza {name: 'Fire Engine Pizza Company', price: 2, rating: 4}),
        (p7:Pizza {name: 'Michael’s Pizzeria Restaurant', price: 2, rating: 4.5}),
        (p8:Pizza {name: 'Pieology Pizzeria', price: 2, rating: 5}),
        (p9:Pizza {name: 'Romanacci Pizza Bar', price: 2, rating: 4}),
        (p0:Pizza {name: 'Romanacci', price: 2, rating: 4})
WITH p1,p2,p3,p4,p5,p6,p7,p8,p9,p0
MATCH (s1:Settlement {name: 'Danbury'}),
        (s2:Settlement {name: 'Fairfield'}),
        (s3:Settlement {name: 'Norwalk'}),
        (s4:Settlement {name: 'Bridgeport'}),
        (s5:Settlement {name: 'Stamford'}),
        (s6:Settlement {name: 'Trumbull'})
MERGE (p1)-[:LOCATED_IN]->(s1)
MERGE (p2)-[:LOCATED_IN]->(s2)
MERGE (p3)-[:LOCATED_IN]->(s1)
MERGE (p4)-[:LOCATED_IN]->(s3)
MERGE (p5)-[:LOCATED_IN]->(s3)
MERGE (p6)-[:LOCATED_IN]->(s4)
MERGE (p7)-[:LOCATED_IN]->(s5)
MERGE (p8)-[:LOCATED_IN]->(s5)
MERGE (p9)-[:LOCATED_IN]->(s6)
MERGE (p0)-[:LOCATED_IN]->(s3)

// Same as previous creation statement except for a single pizza place
CREATE (p1:Pizza {name: 'Stanziato’s Wood Fired Pizza', price: 2, rating: 4.5})
WITH p1
MATCH (s1:Settlement {name: 'Danbury'})
MERGE (p1)-[:LOCATED_IN]->(s1)

// Create Indexes on labels that we search for by a specific name
CREATE INDEX ON :Settlement(name)
CREATE INDEX ON :Pizza(name)