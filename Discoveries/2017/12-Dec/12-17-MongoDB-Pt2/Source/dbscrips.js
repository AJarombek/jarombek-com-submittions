// Author: Andrew Jarombek
// Date: 12/17/2017
//  More MongoDB JavaScript Shell commands for the christmas tree database

db.tree.findOne({type:"douglas", grade:"7-8ft", height:"7' 3\""})
/*{
	"_id" : ObjectId("5a3352702e48ee76cb1fe459"),
	"type" : "douglas",
	"height" : "7' 3\"",
	"source_price" : 10,
	"sell_price" : 60,
	"grade" : "7-8ft",
	"sold" : false,
	"buyer_id" : undefined
}*/

db.customer.insert({
    username: "andy",
    name: "Andrew Jarombek",
    email: "andy@jarombek.com"
})

let tree_id = db.tree.findOne({type:"douglas", grade:"7-8ft", height:"7' 3\""})._id;
let user_id = db.customer.findOne()._id;

db.purchase.insert({
    type: "douglas",
    grade: "7-8ft",
    price: 60,
    tree_id: tree_id,
    username: "andy",
    user_id: user_id,
    date: Date()
})

db.purchase.findOne()
/*{
	"_id" : ObjectId("5a34a1942e48ee76cb1fe82c"),
	"type" : "douglas",
	"grade" : "7-8ft",
	"price" : 60,
	"tree_id" : ObjectId("5a3352702e48ee76cb1fe459"),
	"username" : "andy",
	"user_id" : ObjectId("5a349e732e48ee76cb1fe82b"),
	"date" : "Fri Dec 15 2017 23:31:16 GMT-0500 (EST)"
}*/

db.tree.find({type:"douglas", grade:"7-8ft", height:"7' 3\""}).explain("executionStats")
/*{
	"executionStats" : {
		"executionSuccess" : true,
		"nReturned" : 6,
		"executionTimeMillis" : 4,
		"totalKeysExamined" : 0,
		"totalDocsExamined" : 1003,
    }
}*/

db.tree.createIndex({type: 1})
db.tree.createIndex({grade: 1})
db.tree.createIndex({height: 1})

// Four indexes - our created ones and the default one on _id
db.tree.getIndexes()

db.tree.updateMany({}, {$set: {"availableUntil": new Date("2017-12-24")}})

// The tree documents will expire on the date in the availableUntil property 
db.tree.createIndex({availableUntil: 1}, {expireAfterSeconds: 0})