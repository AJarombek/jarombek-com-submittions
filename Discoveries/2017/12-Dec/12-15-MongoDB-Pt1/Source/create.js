// Author: Andrew Jarombek
// Date: 12/13/2017
// MongoDB JavaScript Shell commands for creating a christmas tree database

db.tree.insert({type: "balsam"})

// Update with $set.  You can perform multiple updates
db.tree.update({type: "balsam"}, {   
    $set: {     
        height: "6'11\"",
        source_price: 10.50,
        sell_price: 45.00,
        grade: "6-7ft"
    } 
})

// Create an empty function which can be edited in vim
function createTree() {}

function createTree() {
    db.tree.insert({
        type: "frazier",
        height: "6'1\"",
        source_price: 14.00,
        sell_price: 45.00,
        grade: "6-7ft"
    })
}

// Show all the documents inside the tree collection - shows the balsam tree
db.tree.find().pretty()

// Call the createTree function to insert the frazier tree
createTree()

// update() only updates the first item that matches.  To update all that match, use updateMany()
db.tree.updateMany({}, {$set: {"sold": false}})
// { "acknowledged" : true, "matchedCount" : 2, "modifiedCount" : 2 }

db.tree.updateMany({}, {$set: {"buyer_id": undefined}})
// { "acknowledged" : true, "matchedCount" : 2, "modifiedCount" : 2 }

// Create an empty function for vim - will bulk insert xmas trees into the database
function bulkTreeInsert() {}

function bulkTreeInsert(number=1) {
    let types = [
        {type: "frazier", grade: "3-4ft", feet: 3, source: 7, sell: 25},
        {type: "frazier", grade: "4-5ft", feet: 4, source: 7.50, sell: 30},
        {type: "frazier", grade: "5-6ft", feet: 5, source: 8, sell: 45},
        {type: "frazier", grade: "6-7ft", feet: 6, source: 9.50, sell: 55},
        {type: "frazier", grade: "7-8ft", feet: 7, source: 10.50, sell: 65},
        {type: "frazier", grade: "8-9ft", feet: 8, source: 12, sell: 85},
        {type: "frazier", grade: "9-10ft", feet: 9, source: 15, sell: 115},
        {type: "frazier", grade: "10+ft", feet: 10, source: 20, sell: 140},
        {type: "balsam", grade: "5-6ft", feet: 5, source: 7, sell: 30},
        {type: "balsam", grade: "6-7ft", feet: 6, source: 8, sell: 40},
        {type: "balsam", grade: "7-8ft", feet: 7, source: 9, sell: 50},
        {type: "balsam", grade: "8-9ft", feet: 8, source: 10, sell: 65},
        {type: "balsam", grade: "9-10ft", feet: 9, source: 11.50, sell: 80},
        {type: "douglas", grade: "5-6ft", feet: 5, source: 7.50, sell: 40},
        {type: "douglas", grade: "6-7ft", feet: 6, source: 8.50, sell: 50},
        {type: "douglas", grade: "7-8ft", feet: 7, source: 10, sell: 60}
    ];

    for (let i = 0; i < number; i++) {

        // Get a random index in the types array
        let random = Math.floor(Math.random() * 16);

        // Get a random number for inches between 0-11
        let inches = Math.floor(Math.random() * 11);

        let selected = types[random];
        let tree = {
            type: selected["type"],
            height: `${selected["feet"]}' ${inches}"`,
            source_price: selected["source"],
            sell_price: selected["sell"],
            grade: selected["grade"],
            sold: false,
            buyer_id: undefined
        };

        db.tree.insert(tree)
    }
}

// Insert one tree
bulkTreeInsert();

// Insert 1000 trees
bulkTreeInsert(1000);

bd.tree.count() // 1003

// Get the number of trees returned by the find() query
db.tree.find({type:"frazier", grade:"6-7ft"}).count()