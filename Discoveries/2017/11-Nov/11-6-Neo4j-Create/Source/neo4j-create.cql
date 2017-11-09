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