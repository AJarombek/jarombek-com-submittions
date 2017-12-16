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