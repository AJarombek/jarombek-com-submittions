// Author: Andrew Jarombek
// Date: 10/27/2017
// Create a model of fairfield county connecticut

CREATE (ct:State {name: 'Connecticut'}) RETURN ct

// Create all the cities in Fairfield county as nodes
CREATE (:City {name: 'Bridgeport'}),
    (:City {name: 'Danbury'}),
    (:City {name: 'Norwalk'}),
    (:City {name: 'Shelton'}),
    (:City {name: 'Stamford'})

// Create all the towns in Fairfield county as nodes
CREATE (:Town {name: 'Bethel'}),
    (:Town {name: 'Brookfield'}),
    (:Town {name: 'Darien'}),
    (:Town {name: 'Easton'}),
    (:Town {name: 'Fairfield'}),
    (:Town {name: 'Greenwich'}),
    (:Town {name: 'Monroe'}),
    (:Town {name: 'New Canaan'}),
    (:Town {name: 'New Fairfield'}),
    (:Town {name: 'Newtown'}),
    (:Town {name: 'Redding'}),
    (:Town {name: 'Ridgefield'}),
    (:Town {name: 'Sherman'}),
    (:Town {name: 'Stratford'}),
    (:Town {name: 'Trumbull'}),
    (:Town {name: 'Weston'}),
    (:Town {name: 'Westport'}),
    (:Town {name: 'Wilton'})

// Group all Cities and Towns under the label Settlement
MATCH (s) WHERE s:City OR s:Town SET s:Settlement

// Create a relationship from each town to each state
MATCH (ct:State), (s:Settlement) MERGE (ct)<-[:IN]-(s)

// Create all of the neighbor relationships among towns/cities
MATCH (greenwich:Settlement {name: 'Greenwich'}),
    (bethel:Settlement {name: 'Bethel'}),
    (brookfield:Settlement {name: 'Brookfield'}),
    (darien:Settlement {name: 'Darien'}),
    (easton:Settlement {name: 'Easton'}),
    (fairfield:Settlement {name: 'Fairfield'}),
    (monroe:Settlement {name: 'Monroe'}),
    (newcannan:Settlement {name: 'New Canaan'}),
    (newfairfield:Settlement {name: 'New Fairfield'}),
    (newtown:Settlement {name: 'Newtown'}),
    (redding:Settlement {name: 'Redding'}),
    (ridgefield:Settlement {name: 'Ridgefield'}),
    (sherman:Settlement {name: 'Sherman'}),
    (stratford:Settlement {name: 'Stratford'}),
    (trumbull:Settlement {name: 'Trumbull'}),
    (weston:Settlement {name: 'Weston'}),
    (westport:Settlement {name: 'Westport'}),
    (wilton:Settlement {name: 'Wilton'}),
    (bridgeport:Settlement {name: 'Bridgeport'}),
    (danbury:Settlement {name: 'Danbury'}),
    (norwalk:Settlement {name: 'Norwalk'}),
    (shelton:Settlement {name: 'Shelton'}),
    (stamford:Settlement {name: 'Stamford'})
CREATE (greenwich)-[:NEIGHBORS_OF]->(stamford),
    (stamford)-[:NEIGHBORS_OF]->(newcannan),
    (stamford)-[:NEIGHBORS_OF]->(darien),
    (darien)-[:NEIGHBORS_OF]->(norwalk),
    (darien)-[:NEIGHBORS_OF]->(newcannan),
    (newcannan)-[:NEIGHBORS_OF]->(wilton),
    (newcannan)-[:NEIGHBORS_OF]->(norwalk),
    (norwalk)-[:NEIGHBORS_OF]->(wilton),
    (norwalk)-[:NEIGHBORS_OF]->(westport),
    (westport)-[:NEIGHBORS_OF]->(wilton),
    (westport)-[:NEIGHBORS_OF]->(weston),
    (westport)-[:NEIGHBORS_OF]->(fairfield),
    (fairfield)-[:NEIGHBORS_OF]->(weston),
    (fairfield)-[:NEIGHBORS_OF]->(easton),
    (fairfield)-[:NEIGHBORS_OF]->(trumbull),
    (fairfield)-[:NEIGHBORS_OF]->(bridgeport),
    (bridgeport)-[:NEIGHBORS_OF]->(stratford),
    (bridgeport)-[:NEIGHBORS_OF]->(trumbull),
    (stratford)-[:NEIGHBORS_OF]->(trumbull),
    (stratford)-[:NEIGHBORS_OF]->(shelton),
    (wilton)-[:NEIGHBORS_OF]->(ridgefield),
    (wilton)-[:NEIGHBORS_OF]->(redding),
    (wilton)-[:NEIGHBORS_OF]->(weston),
    (weston)-[:NEIGHBORS_OF]->(redding),
    (weston)-[:NEIGHBORS_OF]->(easton),
    (easton)-[:NEIGHBORS_OF]->(redding),
    (easton)-[:NEIGHBORS_OF]->(newtown),
    (easton)-[:NEIGHBORS_OF]->(monroe),
    (easton)-[:NEIGHBORS_OF]->(trumbull),
    (trumbull)-[:NEIGHBORS_OF]->(monroe),
    (trumbull)-[:NEIGHBORS_OF]->(shelton),
    (shelton)-[:NEIGHBORS_OF]->(monroe),
    (ridgefield)-[:NEIGHBORS_OF]->(danbury),
    (ridgefield)-[:NEIGHBORS_OF]->(redding),
    (redding)-[:NEIGHBORS_OF]->(danbury),
    (redding)-[:NEIGHBORS_OF]->(bethel),
    (redding)-[:NEIGHBORS_OF]->(newtown),
    (monroe)-[:NEIGHBORS_OF]->(newtown),
    (danbury)-[:NEIGHBORS_OF]->(newfairfield),
    (danbury)-[:NEIGHBORS_OF]->(brookfield),
    (danbury)-[:NEIGHBORS_OF]->(bethel),
    (bethel)-[:NEIGHBORS_OF]->(brookfield),
    (bethel)-[:NEIGHBORS_OF]->(newtown),
    (brookfield)-[:NEIGHBORS_OF]->(newfairfield),
    (newfairfield)-[:NEIGHBORS_OF]->(sherman)

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